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
import probably.*

object Tests extends Suite(t"Aristotle tests"):
  test(t"Matrix"):
    math"[[a\tb\nc\nd]]"
  
  test(t"Column vector"):
    math"(a\nb)"
  
  test(t"Augmented matrix"):
    math"[[[a b\nc d]|[c\nd]]]"
  
  test(t"Subscripts"):
    math"lim_[N→∞]sum_[i=0]^N"
  
  test(t"Derivatives"):
    math"f'(x)=[dy]/[dx]"
  
  test(t"Integration"):
    math"∫_0^1f(x)[dx]"
  
  test(t"Grouping"):
    math"[1+2+3+4]⏟[4 terms]"
