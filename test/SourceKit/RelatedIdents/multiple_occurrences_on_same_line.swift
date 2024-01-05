// REQUIRES: swift_swift_parser
// RUN: %sourcekitd-test -req=related-idents -pos=%(line + 1):5 %s -- %s | %FileCheck %s
let asd = [0]
let index = asd.firstIndex(of: 0)!
print(asd[asd.firstIndex(of: 0)!...])


// CHECK: START RANGES
// CHECK: 3:5 - 3
// CHECK: 4:13 - 3
// CHECK: 5:7 - 3
// CHECK: 5:11 - 3
// CHECK: END RANGES

