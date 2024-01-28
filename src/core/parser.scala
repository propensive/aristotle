package aristotle

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*
import dendrology.*, treeStyles.default

import scala.annotation.*

import perforate.*, errorHandlers.throwUnsafely
import spectacular.*

import unsafeExceptions.canThrowAny

case class Math(mathMl: MathMl*)

object Math:
  def complement(char: Char): Char = char match
    case '[' => ']'
    case '(' => ')'
    case '{' => '}'
    case '<' => '>'
    case '«' => '»'

  def parse(text: Text): Tree =
    val Open = text(0)
    val Close = complement(Open)
    var index: Int = 1

    def recur(current: Tree = Leaf): Tree =
      text(index).also(index += 1) match
        case Open  => current match
          case Leaf                   => recur(Op(recur(Leaf), Unset, Leaf))
          case Op(left, Unset, right) => recur(Op(left, '×', recur(right)))
          case Op(left, node, right)  => recur(Op(left, node, recur(right)))
        
        case Close => current
        
        case char  => current match
          case Leaf                   => recur(Op(Op(Leaf, char, Leaf), Unset, Leaf))
          case Op(left, Unset, right) => recur(Op(left, char, right))
          case Op(left, node, Leaf)   => recur(Op(left, node, Op(Leaf, char, Leaf)))
          case Op(left, node, right)  =>
            index -= 1
            current
      
    recur()

  def tree(text: Text): Unit =
    println(parse(text).show)
    TreeDiagram[Tree](parse(text)).render(_.show).foreach(println(_))

object Tree:
  given Expandable[Tree] =
    case Leaf => Nil
    case Op(left, value, right) => List(left, right).filter(_ != Leaf)

  given Show[Tree] =
    case Leaf                   => t""
    case Op(left, value, right) => t"$left${value.let(_.show).or(t"")}$right"

enum Tree:
  case Leaf
  case Op(left: Tree, operator: Optional[Char], right: Tree)

export Tree.*

enum MathMl:
  case Error()
  case Frac()
  case Identifier()
  case Multiscripts()
  case Numeric()
  case Operator()
  case Over()
  case Padded()
  case Phantom()
  case Root()
  case Row()
  case String()
  case Space()
  case Sqrt()
  case Sub()
  case SupSub()
  case Sup()
  case Table(rows: List[MathMl.Tr])
  case Text()
  case Tr()
  case Under()
  case UnderOver()