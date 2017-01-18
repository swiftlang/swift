struct S {
  func getArray() -> [S] { return [] }
  func getInstance() -> S { return self }
  static var Instance : S = S()
}

class C {
  func getArray() -> [C] { return [] }
  func getInstance() -> C { return self }
  static var Instance : C = C()
}

enum E {
  case CASE1
  func getArray() -> [E] { return [] }
  func getInstance() -> E { return self }
  static var Instance : E = E.CASE1
}
func SGen() -> S { return S() }
func CGen() -> C { return C() }
func EGen() -> E { return .CASE1}

func foo(s : S, c : C, e: E) {
  _ = s.getArray()
  _ = s.getInstance()
  _ = S.Instance
  _ = c.getArray()
  _ = c.getInstance()
  _ = C.Instance
  _ = e.getInstance()
  _ = e.getArray()
  _ = E.CASE1
  _ = E.Instance
  _ = SGen().getArray()
  _ = CGen().getInstance()
  _ = EGen().getArray()
  _ = SArrayGen().count
}

func SArrayGen() -> [S] { return [] }

// RUN: %sourcekitd-test -req=cursor -pos=24:12 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=cursor -pos=25:12 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=cursor -pos=34:19 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: <Container>_TtV21cursor_info_container1S</Container>

// RUN: %sourcekitd-test -req=cursor -pos=26:12 %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: <Container>_TtMV21cursor_info_container1S</Container>

// RUN: %sourcekitd-test -req=cursor -pos=27:12 %s -- %s | %FileCheck -check-prefix=CHECK3 %s
// RUN: %sourcekitd-test -req=cursor -pos=28:12 %s -- %s | %FileCheck -check-prefix=CHECK3 %s
// RUN: %sourcekitd-test -req=cursor -pos=35:19 %s -- %s | %FileCheck -check-prefix=CHECK3 %s
// CHECK3: <Container>_TtC21cursor_info_container1C</Container>

// RUN: %sourcekitd-test -req=cursor -pos=29:12 %s -- %s | %FileCheck -check-prefix=CHECK4 %s
// CHECK4: <Container>_TtMC21cursor_info_container1C</Container>

// RUN: %sourcekitd-test -req=cursor -pos=30:12 %s -- %s | %FileCheck -check-prefix=CHECK5 %s
// RUN: %sourcekitd-test -req=cursor -pos=31:12 %s -- %s | %FileCheck -check-prefix=CHECK5 %s
// RUN: %sourcekitd-test -req=cursor -pos=36:19 %s -- %s | %FileCheck -check-prefix=CHECK5 %s
// CHECK5: <Container>_TtO21cursor_info_container1E</Container>

// RUN: %sourcekitd-test -req=cursor -pos=32:12 %s -- %s | %FileCheck -check-prefix=CHECK6 %s
// RUN: %sourcekitd-test -req=cursor -pos=33:12 %s -- %s | %FileCheck -check-prefix=CHECK6 %s
// CHECK6: <Container>_TtMO21cursor_info_container1E</Container>

// RUN: %sourcekitd-test -req=cursor -pos=37:22 %s -- %s | %FileCheck -check-prefix=CHECK7 %s
// CHECK7: <Container>_TtGSaV21cursor_info_container1S_</Container>
