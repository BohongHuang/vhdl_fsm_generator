package org.coco24.vhdlfsmgen.tree

import scala.annotation.targetName

extension(string: String) {
  def mkIndent(indent: Int): String = {
    if(indent > 0) 
      string.split('\n').map(line => Array.fill(indent)("  ").mkString + line).mkString("\n")
    else string
  }
  def mkNewLine: String = if(!string.isBlank) s"\n$string" else string
}

trait Tree[+T] {
  def show: String
}

trait Sentence[+T] extends Tree[T]

case class Port(ports: Seq[Port.Element]) extends Sentence[Unit] {
  extension(element: Port.Element) @targetName("showElement") inline def show: String = {
    val (names, direction, tpe) = element
    s"""${names.mkString(", ")} : ${direction.show} ${tpe.show}"""
  }
  override def show: String = s"""port(${ports.map(_.show).mkString(";\n     ")});"""
}

object Port {
  type Element = (Seq[String], Direction, Type)
  enum Direction {
    case Input, Output
    def show: String = this match {
      case Input => "in"
      case Output => "out"
    }
  }
}

case class Entity(name: String, body: Tree[Unit]) extends org.coco24.vhdlfsmgen.tree.Sentence[Unit] {
  override def show: String = s"""|entity ${name} is
                                  |${body.show.mkIndent(1)}
                                  |end ${name};""".stripMargin
}

case class If(`if`: (Tree[Boolean], Sentence[Any]), elsif: Seq[(Tree[Boolean], Sentence[Any])] = Seq.empty, `else`: Option[Sentence[Any]] = None) extends Sentence[Unit] {
  override def show: String = {
    val (ifCondition, ifBranch) = `if`
    var string = s"""|if ${ifCondition.show} then
                     |${ifBranch.show.mkIndent(1)}
                     |""".stripMargin
    for((elsifCondition, elsifBranch) <- elsif) {
      string += s"""|elsif ${elsifCondition.show} then
                    |${elsifBranch.show.mkIndent(1)}
                    |""".stripMargin
    }
    `else`.foreach { elseBranch =>
      string += s"""|else
                    |${elseBranch.show.mkIndent(1)}
                    |""".stripMargin
    }
    string += "end if;"
    string
  }
}

case class OnEvent(value: Tree[Any]) extends Tree[Boolean] {
  override def show: String = s"""${value.show}'event"""
}

case class When[+T](value: Tree[T], body: Sentence[Any]) extends Sentence[Unit] {
  override def show: String = body match {
    case body @ Block(elems) if(elems.size > 1) =>
      s"""|when ${value.show} =>
          |${body.show.mkIndent(1)}""".stripMargin
    case body => s"""when ${value.show} => ${body.show}"""
  }
}

object When {
  object Others extends Tree[?] {
    override def show: String = "others"
  }
  def others[T](body: Sentence[Any]): When[T] = When[T](Others.asInstanceOf[Tree[T]], body)
}

case class Case[T](value: Tree[T], branches: Seq[When[T]]) extends Sentence[Unit] {
  override def show: String = {
    s"""|case ${value.show} is
        |${branches.map(_.show.mkIndent(1)).mkString("\n")}
        |end case;""".stripMargin
  }
}

object Case {
  def apply[T](value: Tree[T], branches: Seq[When[T]], others: Sentence[Any]): Case[T] = apply[T](value, branches.appended(When.others(others)))
}

case class DelayAssignment[T](variable: Tree[T], value: Tree[T]) extends Sentence[Unit] {
  override def show: String = {
    s"""${variable.show} <= ${value.show};"""
  }
}

case class Variable[T](identifier: String) extends Tree[T] {
  override def show: String = identifier
}

case class Block(trees: Seq[Sentence[Any]]) extends Sentence[Unit] {
  override def show: String = trees.map(_.show).mkString("\n")
}

sealed trait Type extends Tree[?]

object Type {
  case class Named(name: String) extends Type {
    override def show: String = name
  }
  
  case class Enum(types: Seq[Type]) extends Type {
    override def show: String = types.map(_.show).mkString("(", ", ", ")")
  }

  case class StandardLogicVector(range: Range) extends Type {
    override def show: String = s"""std_logic_vector(${range.show})"""
  }

  case object StandardLogic extends Type {
    override def show: String = s"""std_logic"""
  }

  enum Range {
    case To(from: Int, to: Int)
    case DownTo(from: Int, to: Int)
    def show: String = this match {
      case DownTo(from, to) => s"""${from} downto ${to}"""
      case To(from, to) => s"""${from} to ${to}"""
    }
  }

  case class Integer(range: Option[Range]) extends Type {
    override def show: String = range match {
      case Some(range) => s"""integer range ${range.show}"""
      case None => "integer"
    }
  }
}

case class TypeAlias(name: String, `type`: Type) extends Sentence[Unit] {
  override def show: String = s"""type ${name} is ${`type`.show};"""
}

case class SignalDefinition(names: Seq[String], `type`: Type) extends Sentence[Unit] {
  override def show: String = s"""signal ${names.mkString(", ")}: ${`type`.show};"""
}

object SignalDefinition {
  def apply(name: String, `type`: Type): SignalDefinition = apply(Seq(name), `type`)
}

case class ArchitectureDefinition(architectureName: String, entityName: String, predef: Sentence[Any], body: Sentence[Any]) extends Sentence[Unit] {
  override def show: String = s"""|architecture ${architectureName} of ${entityName} is ${predef.show.mkIndent(1).mkNewLine}
                                  |begin
                                  |${body.show.mkIndent(1)}
                                  |end ${architectureName};""".stripMargin
}

case class ProcessDefinition(processName: String, sensitiveList: Seq[Variable[?]], body: Sentence[Any]) extends Sentence[Any] {
  override def show: String = s"""|${processName}: process(${sensitiveList.map(_.show).mkString(", ")})
                                  |begin
                                  |${body.show.mkIndent(1)}
                                  |end process ${processName};""".stripMargin
}

object Block {
  @targetName("applyRep")
  def apply(trees: Sentence[Any]*): Block = apply(trees)
  @targetName("applyIter")
  def apply(iterable: IterableOnce[Sentence[Any]]): Block = apply(iterable.toSeq)
}

enum Logic {
  case High, Low, Z
}

object Logic {
  given Conversion[Boolean, Logic] with {
    override def apply(bool: Boolean): Logic = bool match {
      case true  => Logic.High
      case false => Logic.Low
    }
  }
}

case class LogicLiteral(value: Logic) extends Tree[Boolean] {
  override def show: String = (value match {
    case Logic.High => "1"
    case Logic.Low  => "0"
    case Logic.Z    => "Z"
  }).prepended('\'').appended('\'')
}

case class LogicVectorLiteral(value: Seq[Logic]) extends Tree[Seq[Boolean]] {
  override def show: String = value.map {
    case Logic.High => '1'
    case Logic.Low  => '0'
    case Logic.Z    => 'Z'
  }.mkString.prepended('"').appended('"')
}

object LogicVectorLiteral {
  def apply(string: String): LogicVectorLiteral = apply(string.map {
    case '1' => Logic.High
    case '0' => Logic.Low
    case 'Z' => Logic.Z
  })
}

case class IntegerLiteral(value: Int) extends Tree[Int] {
  override def show: String = s"""${value}"""
}

case class Equal[T](lvalue: Tree[T], rvalue: Tree[T]) extends Tree[Boolean] {
  override def show: String = s"""${lvalue.show} = ${rvalue.show}"""
}

case class Cons[T](lvalue: Tree[T], rvalue: Tree[T]) extends Tree[Boolean] {
  override def show: String = s"""${lvalue.show} & ${rvalue.show}"""
}

case class Library(path: List[String]) extends Sentence[Unit] {
  override def show: String = s"""library ${path.mkString(".")};"""
}

case class And(left: Tree[Boolean], right: Tree[Boolean]) extends Tree[Boolean] {
  extension(tree: Tree[Boolean]) def showWrapped: String = tree.show
  override def show: String = s"""${left.showWrapped} and ${right.showWrapped}"""
}

case class Use(path: List[String], selector: Use.Selector) extends Sentence[Unit] {
  override def show: String = s"""use ${path.mkString(".")}.${selector.show};"""
}

object Use {
  enum Selector {
    case All
    case One(name: String)
    def show: String = this match {
      case All => "all"
      case One(name) => s"${name}"
    }
  }
}

@main
def test() = {

}
