/*
    , version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package aristotle

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*
import dendrology.*, treeStyles.default

import scala.annotation.*

import contingency.*, strategies.throwUnsafely
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
    def next(): Optional[Char] = if index >= text.length then Unset else text(index).also(index += 1)

    given Show[Optional[Char]] =
      case Unset => t""
      case char  => char.toString.tt

    def recur(current: Tree = Leaf): Tree = next() match
      case Unset =>
        current

      case Open  => current match
        case Leaf                  => recur(Op(recur(Leaf), t"", Leaf))
        case Op(left, t"", right)  => recur(Op(left, t"×", recur(right)))
        case Op(left, node, right) => recur(Op(left, node, recur(right)))

      case Close => current

      case char: Char => current match
        case Leaf                  => recur(Op(Op(Leaf, char.show, Leaf), t"", Leaf))
        case Op(left, t"", right)  => recur(Op(left, char.show, right))
        case Op(left, node, Leaf)  =>
          if char.isLetter == node.head.isLetter then recur(Op(left, t"$node$char", Leaf))
          else recur(Op(left, node, Op(Leaf, char.show, Leaf)))
        case Op(left, node, right) => recur(Op(current, char.show, Leaf))

    recur().also:
      if index != text.length then ???


  def tree(text: Text): Unit =
    println(parse(text).show)
    TreeDiagram[Tree](parse(text)).render(_.show).foreach(println(_))

object Tree:
  given Expandable[Tree] =
    case Leaf                   => Nil
    case Op(left, value, right) => List(left, right).filter(_ != Leaf)

  given Show[Tree] =
    case Leaf                   => t""
    case Op(left, value, right) => t"$left${value.let(_.show).or(t"")}$right"

enum Tree:
  case Leaf
  case Op(left: Tree, operator: Text, right: Tree)

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
