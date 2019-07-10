// RUN: %swift-format %s >%t.response
// RUN: diff -u %s.response %t.response
// RUN: %swift-format -indent-width 2 %s >%t.response
// RUN: diff -u %s.indent2.response %t.response
// RUN: %swift-format -use-tabs %s >%t.response
// RUN: diff -u %s.tabs.response %t.response
// RUN: %swift-format -line-range 24:29 %s >%t.response
// RUN: diff -u %s.lines.response %t.response
// RUN: %swift-format -indent-switch-case %s >%t.response
// RUN: diff -u %s.indentswitch.response %t.response

import Foundation

func collatz(n: Int, m: String?) {
switch m {
    case .some(let s):
            print(s)
case .none:
print("nothing")
default:
    print("not possible")
break
    }
 var r: Int
    if n%2 == 0 {
          r = n/2
        } else {
            r = 3*n+1
    }
       print("Number: \(r)")
       if r == 1 {
           print("Reached one!")
  } else {
           collatz(r)
    }
}
