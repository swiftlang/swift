class AL : ExpressibleByArrayLiteral {
  typealias Element = Int
  public required init(arrayLiteral elements: Element...) {
    var sum = 0
    for s in elements { sum = sum + s }
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=5:18 %s -- %s | %FileCheck %s
// CHECK: [<Type usr="s:C12cursor_array2AL">AL</Type>.<Type usr="s:C12cursor_array2AL7Element">Element</Type>]