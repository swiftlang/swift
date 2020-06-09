// RUN: %complete-test %s -group=none -fuzz -structure -tok=S1_DOT | %FileCheck %s -check-prefix=S1_DOT
// RUN: %complete-test %s -group=none -add-inner-results -fuzz -structure -tok=S1_POSTFIX | %FileCheck %s -check-prefix=S1_POSTFIX
// RUN: %complete-test %s -group=none -add-inner-results -fuzz -structure -tok=S1_POSTFIX_INIT | %FileCheck %s -check-prefix=S1_INIT
// RUN: %complete-test %s -group=none -fuzz -structure -tok=S1_PAREN_INIT | %FileCheck %s -check-prefix=S1_INIT
// RUN: %complete-test %s -group=none -hide-none -fuzz -structure -tok=STMT_0 | %FileCheck %s -check-prefix=STMT_0
// RUN: %complete-test %s -group=none -fuzz -structure -tok=ENUM_0 | %FileCheck %s -check-prefix=ENUM_0
// RUN: %complete-test %s -group=none -fuzz -structure -tok=OVERRIDE_0 | %FileCheck %s -check-prefix=OVERRIDE_0
// RUN: %complete-test %s -group=none -fuzz -structure -tok=S1_INNER_0 | %FileCheck %s -check-prefix=S1_INNER_0
// RUN: %complete-test %s -group=none -fuzz -structure -tok=INT_INNER_0 | %FileCheck %s -check-prefix=INT_INNER_0
// RUN: %complete-test %s -group=none -fuzz -structure -tok=ASSOCIATED_TYPE_1 | %FileCheck %s -check-prefix=ASSOCIATED_TYPE_1

struct S1 {
  func method1() {}
  func method2(_ a: Int, b: Int) -> Int { return 1 }
  func method3(a a: Int, b: Int) {}
  func method4(_: Int, _: Int) {}
  func method5(_: inout Int, b: inout Int) {}
  func method6(_ c: Int) throws {}
  func method7(_ callback: () throws -> ()) rethrows {}
  func method8<T, U>(_ d: (T, U) -> T, e: T -> U) {}

  let v1: Int = 1
  var v2: Int { return 1 }

  subscript(x: Int, y: Int) -> Int { return 1 }
  subscript(x x: Int, y y: Int) -> Int { return 1 }

  init() {}
  init(a: Int, b: Int) {}
  init(_: Int, _: Int) {}
  init(c: Int)? {}
}

func test1(_ x: S1) {
  x.#^S1_DOT^#
}
// S1_DOT: {name:method1}()
// S1_DOT: {name:method2}({params:{l:a:}{t: Int}, {n:b:}{t: Int}})
// S1_DOT: {name:method3}({params:{n:a:}{t: Int}, {n:b:}{t: Int}})
// S1_DOT: {name:method4}({params:{t:Int}, {t:Int}})
// S1_DOT: {name:method5}({params:{t:&Int}, {n:b:}{t: &Int}})
// FIXME: put throws in a range!
// S1_DOT: {name:method6}({params:{l:c:}{t: Int}}){throws: throws}
// S1_DOT: {name:method7}({params:{l:callback:}{t: () throws -> ()}}){throws: rethrows}
// S1_DOT: {name:method8}({params:{l:d:}{t: (T, U) -> T}, {n:e:}{t: (T) -> U}})
// S1_DOT: {name:v1}
// S1_DOT: {name:v2}

func test2(_ x: S1) {
  x#^S1_POSTFIX^#
}
// Subscripts!
// S1_POSTFIX: {name:.}
// S1_POSTFIX: [{params:{l:x:}{t: Int}, {l:y:}{t: Int}}]
// S1_POSTFIX: [{params:{n:x:}{t: Int}, {n:y:}{t: Int}}]
// The dot becomes part of the name
// S1_POSTFIX: {name:.method1}()
// S1_POSTFIX: {name:.method2}({params:{l:a:}{t: Int}, {n:b:}{t: Int}})

func test4() {
  S1#^S1_POSTFIX_INIT^#
}
func test5() {
  S1(#^S1_PAREN_INIT^#
}
// S1_INIT: ({params:{t:Int}, {t:Int}})
// S1_INIT: ({params:{n:a:}{t: Int}, {n:b:}{t: Int}})
// S1_INIT: ({params:{n:c:}{t: Int}})

func test6(_ xyz: S1, fgh: (S1) -> S1) {
  #^STMT_0^#
}
// STMT_0: {name:func}
// STMT_0: {name:fgh}
// STMT_0: {name:xyz}
// STMT_0: {name:S1}
// STMT_0: {name:test6}({params:{l:xyz:}{t: S1}, {n:fgh:}{t: (S1) -> S1}})
// STMT_0: {name:try!}

enum E1 {
  case C1
  case C2(S1)
  case C3(l1: S1, l2: S1)
}

func test7(_ x: E1) {
  test7(.#^ENUM_0^#)
}
// ENUM_0: {name:C1}
// ENUM_0: {name:C2}({params:{t:S1}})
// ENUM_0: {name:C3}({params:{n:l1:}{t: S1}, {n:l2:}{t: S1}})

class C1 {
  func foo(x: S1, y: S1, z: (S1) -> S1) -> S1 {}
  func zap<T, U>(x: T, y: U, z: (T) -> U) -> T {}
}

class C2 : C1 {
  override func #^OVERRIDE_0^#
}
// FIXME: overrides don't break out their code completion string structure.
// OVERRIDE_0: {name:foo(x: S1, y: S1, z: (S1) -> S1) -> S1}
// OVERRIDE_0: {name:zap<T, U>(x: T, y: U, z: (T) -> U) -> T}

func test8() {
  #^S1_INNER_0,S1^#
}
// S1_INNER_0: {name:S1.}
// FIXME: should the ( go inside the name here?
// S1_INNER_0: {name:S1}(

func test9(_ x: inout Int) {
  #^INT_INNER_0,x^#
}
// INT_INNER_0: {name:x==}
// INT_INNER_0: {name:x<}
// INT_INNER_0: {name:x+}
// INT_INNER_0: {name:x..<}

protocol P1 {
  associatedtype T
}
struct S2: P1 {
  #^ASSOCIATED_TYPE_1^#
}
// ASSOCIATED_TYPE_1: {name:T = }{params:{l:Type}}
