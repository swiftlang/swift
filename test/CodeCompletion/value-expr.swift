// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 | FileCheck %s -check-prefix=T1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T2 | FileCheck %s -check-prefix=T2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T3 | FileCheck %s -check-prefix=T3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T4 | FileCheck %s -check-prefix=T4

// Test code completion of expressions that produce a value.

struct FooStruct {
  var instanceVar : Int

  func instanceFunc0() {}
  func instanceFunc1(a : Int) {}
  func instanceFunc2(a : Int, b : Double) {}

  func overloadedInstanceFunc1() -> Int {}
  func overloadedInstanceFunc1() -> Double {}

  func overloadedInstanceFunc2(x: Int) -> Int {}
  func overloadedInstanceFunc2(x: Double) -> Int {}

  func curriedVoidFunc1()() {}
  func curriedVoidFunc2()(a: Int) {}
  func curriedVoidFunc3(a: Int)() {}
  func curriedVoidFunc4(a: Int)(b: Int) {}

  func curriedStringFunc1()() -> String {}
  func curriedStringFunc2()(a: Int) -> String {}
  func curriedStringFunc3(a: Int)() -> String {}
  func curriedStringFunc4(a: Int)(b: Int) -> String {}

  static func staticFunc0() {}
  static func staticFunc1(a : Int) {}

  static func overloadedStaticFunc1() -> Int {}
  static func overloadedStaticFunc1() -> Double {}

  static func overloadedStaticFunc2(x: Int) -> Int {}
  static func overloadedStaticFunc2(x: Double) -> Int {}
}

var fooObject : FooStruct
func testObjectExpr() {
  fooObject.#^T1^#
// T1: Begin completions
// T1-NEXT: SwiftDecl: instanceVar[#Int#]{{$}}
// T1-NEXT: SwiftDecl: instanceFunc0()[#()#]{{$}}
// T1-NEXT: SwiftDecl: instanceFunc1({#a: Int#})[#()#]{{$}}
// T1-NEXT: SwiftDecl: instanceFunc2({#a: Int#}, {#b: Double#})[#()#]{{$}}
// T1-NEXT: SwiftDecl: overloadedInstanceFunc1()[#Int#]{{$}}
// T1-NEXT: SwiftDecl: overloadedInstanceFunc1()[#Double#]{{$}}
// T1-NEXT: SwiftDecl: overloadedInstanceFunc2({#x: Int#})[#Int#]{{$}}
// T1-NEXT: SwiftDecl: overloadedInstanceFunc2({#x: Double#})[#Int#]{{$}}
// T1-NEXT: SwiftDecl: curriedVoidFunc1()[#() -> ()#]{{$}}
// T1-NEXT: SwiftDecl: curriedVoidFunc2()[#(a: Int) -> ()#]{{$}}
// T1-NEXT: SwiftDecl: curriedVoidFunc3({#a: Int#})[#() -> ()#]{{$}}
// T1-NEXT: SwiftDecl: curriedVoidFunc4({#a: Int#})[#(b: Int) -> ()#]{{$}}
// T1-NEXT: SwiftDecl: curriedStringFunc1()[#() -> String#]{{$}}
// T1-NEXT: SwiftDecl: curriedStringFunc2()[#(a: Int) -> String#]{{$}}
// T1-NEXT: SwiftDecl: curriedStringFunc3({#a: Int#})[#() -> String#]{{$}}
// T1-NEXT: SwiftDecl: curriedStringFunc4({#a: Int#})[#(b: Int) -> String#]{{$}}
// T1-NEXT: Keyword: metatype{{$}}
// T1-NEXT: End completions
}

func testObjectExprWithoutDot() {
  fooObject#^T2^#
// T2: Begin completions
// T2-NEXT: SwiftDecl: .instanceVar[#Int#]{{$}}
// T2-NEXT: SwiftDecl: .instanceFunc0()[#()#]{{$}}
// T2-NEXT: SwiftDecl: .instanceFunc1({#a: Int#})[#()#]{{$}}
// T2-NEXT: SwiftDecl: .instanceFunc2({#a: Int#}, {#b: Double#})[#()#]{{$}}
// T2-NEXT: SwiftDecl: .overloadedInstanceFunc1()[#Int#]{{$}}
// T2-NEXT: SwiftDecl: .overloadedInstanceFunc1()[#Double#]{{$}}
// T2-NEXT: SwiftDecl: .overloadedInstanceFunc2({#x: Int#})[#Int#]{{$}}
// T2-NEXT: SwiftDecl: .overloadedInstanceFunc2({#x: Double#})[#Int#]{{$}}
// T2-NEXT: SwiftDecl: .curriedVoidFunc1()[#() -> ()#]{{$}}
// T2-NEXT: SwiftDecl: .curriedVoidFunc2()[#(a: Int) -> ()#]{{$}}
// T2-NEXT: SwiftDecl: .curriedVoidFunc3({#a: Int#})[#() -> ()#]{{$}}
// T2-NEXT: SwiftDecl: .curriedVoidFunc4({#a: Int#})[#(b: Int) -> ()#]{{$}}
// T2-NEXT: SwiftDecl: .curriedStringFunc1()[#() -> String#]{{$}}
// T2-NEXT: SwiftDecl: .curriedStringFunc2()[#(a: Int) -> String#]{{$}}
// T2-NEXT: SwiftDecl: .curriedStringFunc3({#a: Int#})[#() -> String#]{{$}}
// T2-NEXT: SwiftDecl: .curriedStringFunc4({#a: Int#})[#(b: Int) -> String#]{{$}}
// T2-NEXT: Keyword: .metatype{{$}}
// T2-NEXT: End completions
}

func testMetatypeExpr() {
  FooStruct.#^T3^#
// T3: Begin completions
// T3-NEXT: SwiftDecl: staticFunc0()[#()#]{{$}}
// T3-NEXT: SwiftDecl: staticFunc1({#a: Int#})[#()#]{{$}}
// T3-NEXT: SwiftDecl: overloadedStaticFunc1()[#Int#]{{$}}
// T3-NEXT: SwiftDecl: overloadedStaticFunc1()[#Double#]{{$}}
// T3-NEXT: SwiftDecl: overloadedStaticFunc2({#x: Int#})[#Int#]{{$}}
// T3-NEXT: SwiftDecl: overloadedStaticFunc2({#x: Double#})[#Int#]{{$}}
// T3-NEXT: Keyword: metatype{{$}}
// T3-NEXT: End completions
}

func testCurriedFunc() {
  fooObject.curriedVoidFunc1()#^T4^#
// T4: Begin completions
// T4-NEXT: Keyword: .metatype{{$}}
// T4-NEXT: End completions
}

