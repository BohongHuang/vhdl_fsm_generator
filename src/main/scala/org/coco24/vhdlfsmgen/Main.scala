package org.coco24.vhdlfsmgen

import java.io.File
import scala.io.Source
import scala.collection
import scala.collection.mutable
import java.util.HashMap
import scala.annotation.tailrec

import tree._
import java.io.FileWriter
import java.nio.charset.Charset

//    (fset 'merge_to_one_arch
//    (kmacro-lambda-form [?\C-e return ?\M-\\ ?\C-s ?a ?r ?c ?h ?i ?t ?e ?c ?t ?u ?r ?e ?  ?n ?u ?m ?_ return ?\C-n ?\C-a ?\C-k ?\C-k ?\C-k ?\C-k ?\C-u ?\C-  ?\C-y backspace ?\C-s ?b ?e ?g ?i ?n return return ?\C-s ?a ?r ?c ?h ?i ?t ?e ?c ?t ?u ?r ?e ?  ?n ?u ?m ?_ return ?\C-n ?\C-n ?\C-a ?\C-  ?\C-s ?e ?n ?d ?  ?p ?r ?o ?c return ?\C-e ?\C-w ?\C-l ?\C-p ?\C-p ?\C-  ?\C-n ?\C-n ?\C-n ?\C-e backspace backspace ?\C-u ?\C-  ?\C-u ?\C-  ?\C-u ?\C-  ?\C-y ?\C-r ?a ?r ?c ?h ?i return] 0 "%d"))


class Counter {
  private var _count: Int = -1
  def count(): Int = {
    _count += 1
    _count
  }

  def peek: Int = _count
}

enum State[K, V, CC[K, V]](val number: Int) {
  case Branch(map: CC[K, State[K, V, CC]])(using counter: Counter) extends State[K, V, CC](counter.count())
  case Leaf(result: V) extends State[K, V, CC](-1)
  def downgrade[DD[K, V] >: CC[K, V]]: State[K, V, DD] = this.asInstanceOf
}

@main def test: Unit = {
  type MutableState = State[String, Int, mutable.HashMap]

  extension(state: MutableState) def flatten: Iterator[MutableState] = state match {
    case State.Branch(map) => Iterator(state) ++ map.values.iterator.map(_.flatten).flatten
    case State.Leaf(_) => Iterator(state)
  }

  def toMermaid(root: MutableState): String = {
    val states = root.flatten;
    // val predef = states.map { state =>
    //   s"""X${state.number}[s${state.number}]"""
    // }.mkString("\n")
    val connections = states.collect {
      case state @ State.Branch(map) =>
        map.map { (pred, target) =>
          if(target.number != -1)
            s"""s${state.number} --> s${target.number}: ${pred}"""
          else
            s"""s${state.number} --> s0: OK"""
        }
    }.flatten.mkString("\n")
    s"""|stateDiagram-v2
        |${connections.mkIndent(1)}""".stripMargin
  }

  extension(string: String) def wrapWithMermaidSrc(fileName: String = string.hashCode.toString): String =
    s"""|#+BEGIN_SRC mermaid :file ${fileName}.png
        |${string.mkIndent(1)}
        |#+END_SRC""".stripMargin

  def toAst(number: Int, root: MutableState): ArchitectureDefinition = {
    val processName = s"proc_num_${number}"
    val stateTypeName = s"state_type_${number}"
    val stateName = s"state_${number}"
    ArchitectureDefinition(s"num_${number}", "number_scanner",
      Block(TypeAlias(stateTypeName, Type.Enum(root.flatten.map(_.number).filter(_ >= 0).map("s" + _).map(Type.Named.apply).toSeq)), SignalDefinition(stateName, Type.Named(stateTypeName))), ProcessDefinition(processName, Seq(Variable("sensor"), Variable("clock"), Variable(stateName), Variable("reset")), If((And(OnEvent(Variable("clock")), Equal(Variable("clock"), LogicLiteral(Logic.High))), If((Equal(Variable("reset"), LogicLiteral(Logic.High)), Block(DelayAssignment(Variable(stateName), Type.Named(s"s${root.number}")), DelayAssignment(Variable("result"), LogicVectorLiteral("ZZZZ")))), Seq.empty, Some(Case(Variable(stateName), root.flatten.collect {
      case state @ State.Branch(map) =>
        When(Type.Named(s"s${state.number}"), Case(Variable("sensor"), map.iterator/*.concat(root.flatten.collect { case State.Branch(map) => map.iterator }.flatten.filter(_._2 == state).distinct.iterator)*/.map {
                    case (pred, state @ State.Branch(_)) => When(LogicVectorLiteral(pred), DelayAssignment(Variable(stateName), Type.Named(s"s${state.number}")))
                    case (pred, state @ State.Leaf(value)) => When(LogicVectorLiteral(pred), Block(DelayAssignment(Variable("result"), LogicVectorLiteral(value.toBinaryString.reverse.padTo(4, '0').reverse)), DelayAssignment(Variable(stateName), Type.Named(s"s${root.number}"))))
        }.toSeq, Block()))
    }.toSeq)))))))
  }

  extension(ast: Iterable[ArchitectureDefinition]) def transformToSingleArchitecture: ArchitectureDefinition = {
    val predefs = ast.map(_.predef)
    val bodys = ast.map(_.body)
    ArchitectureDefinition("main", "number_scanner", Block(predefs), Block(bodys))
  }

  val states = File("res").listFiles.iterator.filter(_.isDirectory).map { numberDirectory =>
        given counter: Counter = Counter()
        val root: MutableState = State.Branch(mutable.HashMap.empty)
        val number = numberDirectory.getName.toInt
        numberDirectory.listFiles.foreach { patternFile =>
          val lines = Source.fromFile(patternFile).getLines.filterNot(_.isEmpty).toArray.view
          // val last = lines.head
          // val iterator = lines.drop(1).reverseIterator
          val last = "11111"
          val iterator = lines.iterator
          @tailrec
          def traverse(state: MutableState): Unit = state match {
            case State.Branch(map) =>
              iterator.nextOption match {
                case Some(value) => traverse(map.getOrElseUpdate(value, State.Branch(mutable.HashMap.empty)))
                case None => map(last) = State.Leaf(number)
              }
            case State.Leaf(_) => throw IllegalStateException("There's already a final state existing!")
          }
          traverse(root)
        }
        number -> root
    }.toMap
  val ast = Block(
    Seq(
      Library("ieee" :: Nil),
      Use("ieee" :: "std_logic_1164" :: Nil, Use.Selector.All),
      Entity("number_scanner", Port(Seq(
      (Seq("sensor"), Port.Direction.Input, Type.StandardLogicVector(Type.Range.DownTo(4, 0))),
      (Seq("clock", "reset"), Port.Direction.Input, Type.StandardLogic),
        (Seq("result"), Port.Direction.Output, Type.StandardLogicVector(Type.Range.DownTo(3, 0)))))),
      // ArchitectureDefinition("reset_handler", "number_scanner", Block(), ProcessDefinition("handle_reset", Seq(Variable("reset"), Variable{"clock"}), If((And(OnEvent(Variable("clock")), Equal(Variable("clock"), LogicLiteral(Logic.High))), If((Equal(Variable("reset"), LogicLiteral(Logic.High)), DelayAssignment(Variable("result"), LogicVectorLiteral("ZZZZ")))))))),
      states.map(toAst).transformToSingleArchitecture))

  val vhdlWriter = FileWriter(File("res/result.vhd"), Charset.defaultCharset, false)
  vhdlWriter.write(ast.show)
  vhdlWriter.close()

  val orgWriter = FileWriter(File("res/diagrams.org"), Charset.defaultCharset, false)
  val orgContent = states.map { (number, state) =>
    s"""|* Number ${number}
        |${toMermaid(state).wrapWithMermaidSrc(number.toString)}""".stripMargin
  }.mkString("\n")
  orgWriter.write(orgContent)
  orgWriter.close()
}

