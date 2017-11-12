class C1 {
  init() {}
}

func test1() {
  var x : C1 = C1()
}

extension C1 {}

test1()

import Swift

class C2 {
  lazy var lazy_bar : Int = {
    let x = 0
    return x
  }()
}

class C2<Param> {
	func f(t : Param) -> Param {return t}
}

// RUN: %sourcekitd-test -req=related-idents -pos=6:17 %s -- -module-name related_idents %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: START RANGES
// CHECK1-NEXT: 1:7 - 2
// CHECK1-NEXT: 6:11 - 2
// CHECK1-NEXT: 6:16 - 2
// CHECK1-NEXT: 9:11 - 2
// CHECK1-NEXT: END RANGES

// RUN: %sourcekitd-test -req=related-idents -pos=5:9 %s -- -module-name related_idents %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: START RANGES
// CHECK2-NEXT: 5:6 - 5
// CHECK2-NEXT: 11:1 - 5
// CHECK2-NEXT: END RANGES

// RUN: %sourcekitd-test -req=related-idents -pos=13:10 %s -- -module-name related_idents %s | %FileCheck -check-prefix=CHECK3 %s
// CHECK3:      START RANGES
// CHECK3-NEXT: END RANGES

// RUN: %sourcekitd-test -req=related-idents -pos=18:12 %s -- -module-name related_idents %s | %FileCheck -check-prefix=CHECK4 %s
// CHECK4:      START RANGES
// CHECK4-NEXT: 17:9 - 1
// CHECK4-NEXT: 18:12 - 1
// CHECK4-NEXT: END RANGES

// RUN: %sourcekitd-test -req=related-idents -pos=22:12 %s -- -module-name related_idents %s | %FileCheck -check-prefix=CHECK5 %s
// CHECK5:      START RANGES
// CHECK5-NEXT: 22:10 - 5
// CHECK5-NEXT: 23:13 - 5
// CHECK5-NEXT: 23:23 - 5
// CHECK5-NEXT: END RANGES
