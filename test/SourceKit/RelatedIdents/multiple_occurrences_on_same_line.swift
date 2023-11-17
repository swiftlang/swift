// RUN: %sourcekitd-test -req=related-idents -pos=%(line + 1):5 %s -- %s | %FileCheck %s
let asd = [0]
let index = asd.firstIndex(of: 0)!
print(asd[asd.firstIndex(of: 0)!...])


// CHECK: START RANGES
// CHECK: 2:5 - 3
// CHECK: 3:13 - 3
// CHECK: 4:7 - 3
// CHECK: 4:11 - 3
// CHECK: END RANGES

