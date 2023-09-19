package aristotle

import anticipation.*
import 

object Tests extends Suite(t"Aristotle tests"):
  test(t"Matrix"):
    math"[[a,b],[c,d]]"
    math"[[a\tb\nc\nd]]"
  
  test(t"Column vector"):
    math"(a\nb)"
  
  test(t"Augmented matrix"):
    math"[[[a\tb\nc\td]|[c\nd]]]"
  
  test(t"Subscripts"):
    math"lim_[N→∞]sum_[i=0]^N"
  
  test(t"Derivatives"):
    math"f'(x)=[dy]/[dx]"
  
  test(t"Integration"):
    math"∫_0^1f(x)[dx]"
  
  test(t"Grouping"):
    math"[1+2+3+4]⏟[4 terms]"
