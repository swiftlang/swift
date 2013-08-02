// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 | FileCheck %s -check-prefix=T1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T2 | FileCheck %s -check-prefix=T2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T3 | FileCheck %s -check-prefix=T3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T4 | FileCheck %s -check-prefix=T4
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T5 | FileCheck %s -check-prefix=T5

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=DOT_DOT_TOKEN_SPLIT | FileCheck %s -check-prefix=DOT_DOT_TOKEN_SPLIT

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CF1 | FileCheck %s -check-prefix=CF1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CF2 | FileCheck %s -check-prefix=CF2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CF3 | FileCheck %s -check-prefix=CF3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CF4 | FileCheck %s -check-prefix=CF4

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_FUNC_0 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_FUNC_0
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_FUNC_1 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_FUNC_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_FUNC_2 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_FUNC_2

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_VARARG_FUNC_0 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_VARARG_FUNC_0
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_VARARG_FUNC_1 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_VARARG_FUNC_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_VARARG_FUNC_2 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_VARARG_FUNC_2

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_OVERLOADED_FUNC_1 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_OVERLOADED_FUNC_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_OVERLOADED_FUNC_2 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_OVERLOADED_FUNC_2

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_CURRIED_FUNC_1 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_CURRIED_FUNC_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_CURRIED_FUNC_2 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_CURRIED_FUNC_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_CURRIED_FUNC_3 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_CURRIED_FUNC_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_CURRIED_FUNC_4 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_CURRIED_FUNC_4

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_INSTANCE_METHOD_1 | FileCheck %s -check-prefix=IN_INSTANCE_METHOD_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STATIC_METHOD_1 | FileCheck %s -check-prefix=IN_STATIC_METHOD_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STATIC_METHOD_1 | FileCheck %s -check-prefix=IN_STATIC_METHOD_1_NEGATIVE

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=VF1 | FileCheck %s -check-prefix=VF1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=VF2 | FileCheck %s -check-prefix=VF2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=BASE_MEMBERS | FileCheck %s -check-prefix=BASE_MEMBERS
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=BASE_MEMBERS_STATIC | FileCheck %s -check-prefix=BASE_MEMBERS_STATIC

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_1 | FileCheck %s -check-prefix=PROTO_MEMBERS_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_2 | FileCheck %s -check-prefix=PROTO_MEMBERS_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_3 | FileCheck %s -check-prefix=PROTO_MEMBERS_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_4 | FileCheck %s -check-prefix=PROTO_MEMBERS_4

// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_NO_DOT_1 | FileCheck %s -check-prefix=SUPER_NO_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_NO_DOT_2 | FileCheck %s -check-prefix=SUPER_NO_DOT_2
// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_NO_DOT_3 | FileCheck %s -check-prefix=SUPER_NO_DOT_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_NO_DOT_4 | FileCheck %s -check-prefix=SUPER_NO_DOT_4
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_DOT_1 | FileCheck %s -check-prefix=SUPER_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_DOT_2 | FileCheck %s -check-prefix=SUPER_DOT_2

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=THIS_DOT_1 | FileCheck %s -check-prefix=THIS_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=THIS_NO_DOT_1 | FileCheck %s -check-prefix=THIS_NO_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=THIS_STATIC_DOT_1 | FileCheck %s -check-prefix=THIS_STATIC_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=THIS_STATIC_NO_DOT_1 | FileCheck %s -check-prefix=THIS_STATIC_NO_DOT_1


// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_1 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_2 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_3 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_4 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_4
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_5 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_5
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_6 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_6
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_7 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_7
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_8 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_8
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_9 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_9

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_1 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_2 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_3 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_4 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_4
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_5 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_5

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_VARARG_FUNCTION_CALL_1 | FileCheck %s -check-prefix=INSIDE_VARARG_FUNCTION_CALL_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_VARARG_FUNCTION_CALL_2 | FileCheck %s -check-prefix=INSIDE_VARARG_FUNCTION_CALL_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_VARARG_FUNCTION_CALL_3 | FileCheck %s -check-prefix=INSIDE_VARARG_FUNCTION_CALL_3

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_1 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_1
// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_2 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_3 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_4 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_4
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_5 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_5
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_6 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_6
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_7 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_7

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_PAREN_PATTERN_1 | FileCheck %s -check-prefix=FUNC_PAREN_PATTERN_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_PAREN_PATTERN_2 | FileCheck %s -check-prefix=FUNC_PAREN_PATTERN_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_PAREN_PATTERN_3 | FileCheck %s -check-prefix=FUNC_PAREN_PATTERN_3

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAINED_CALLS_1 | FileCheck %s -check-prefix=CHAINED_CALLS_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAINED_CALLS_2 | FileCheck %s -check-prefix=CHAINED_CALLS_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAINED_CALLS_3 | FileCheck %s -check-prefix=CHAINED_CALLS_3

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_1 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_2 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_3 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_4 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_4
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_5 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_5
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_ERROR_1 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_ERROR_1

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_MODULES_1 | FileCheck %s -check-prefix=RESOLVE_MODULES_1

// Test code completion of expressions that produce a value.

struct FooStruct {
  var instanceVar : Int

  func instanceFunc0() {}
  func instanceFunc1(a : Int) {}
  func instanceFunc2(a : Int, b : Double) {}

  func varargInstanceFunc0(v : Int...) {}
  func varargInstanceFunc1(a : Float, v : Int...) {}
  func varargInstanceFunc2(a : Float, b : Double, v : Int...) {}

  func overloadedInstanceFunc1() -> Int {}
  func overloadedInstanceFunc1() -> Double {}

  func overloadedInstanceFunc2(x: Int) -> Int {}
  func overloadedInstanceFunc2(x: Double) -> Int {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    instanceVar = i
  }

  subscript(i: Int, j: Int) -> Double {
  get:
    return Double(i + j)
  set(val):
    instanceVar = i + j
  }

  func curriedVoidFunc1()() {}
  func curriedVoidFunc2()(a: Int) {}
  func curriedVoidFunc3(a: Int)() {}
  func curriedVoidFunc4(a: Int)(b: Int) {}

  func curriedStringFunc1()() -> String {}
  func curriedStringFunc2()(a: Int) -> String {}
  func curriedStringFunc3(a: Int)() -> String {}
  func curriedStringFunc4(a: Int)(b: Int) -> String {}

  func selectorVoidFunc1(a: Int) b(x: Float) {}
  func selectorVoidFunc2(a: Int) b(x: Float) c(y: Double) {}

  func selectorStringFunc1(a: Int) b(x: Float) -> String {}
  func selectorStringFunc2(a: Int) b(x: Float) c(y: Double) -> String {}

  struct NestedStruct {}
  class NestedClass {}
  union NestedUnion {}
  // Can not declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int

  // FIXME: uncomment when we have static vars.
  // static var staticVar : Int

  static func staticFunc0() {}
  static func staticFunc1(a : Int) {}

  static func overloadedStaticFunc1() -> Int {}
  static func overloadedStaticFunc1() -> Double {}

  static func overloadedStaticFunc2(x: Int) -> Int {}
  static func overloadedStaticFunc2(x: Double) -> Int {}
}

extension FooStruct {
  var extProp : Int {
  get:
    return 42
  set(val):
  }

  func extFunc0() {}

  static func extStaticFunc0() {}

  struct ExtNestedStruct {}
  class ExtNestedClass {}
  union ExtNestedUnion {
    case ExtUnionX(Int)
  }

  typealias ExtNestedTypealias = Int
}

var fooObject : FooStruct
func testObjectExpr() {
  fooObject.#^T1^#
// T1: Begin completions
// T1-NEXT: SwiftDecl: instanceVar[#Int#]{{$}}
// T1-NEXT: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// T1-NEXT: SwiftDecl: instanceFunc1({#a: Int#})[#Void#]{{$}}
// T1-NEXT: SwiftDecl: instanceFunc2({#a: Int#}, {#b: Double#})[#Void#]{{$}}
//
// FIXME: We lose the '...' sugar on the vararg type.
// Related to rdar://14447454
// T1-NEXT: SwiftDecl: varargInstanceFunc0({#v: Int[]#})[#Void#]{{$}}
// T1-NEXT: SwiftDecl: varargInstanceFunc1({#a: Float#}, {#v: Int[]#})[#Void#]{{$}}
// T1-NEXT: SwiftDecl: varargInstanceFunc2({#a: Float#}, {#b: Double#}, {#v: Int[]#})[#Void#]{{$}}
//
// T1-NEXT: SwiftDecl: overloadedInstanceFunc1()[#Int#]{{$}}
// T1-NEXT: SwiftDecl: overloadedInstanceFunc1()[#Double#]{{$}}
// T1-NEXT: SwiftDecl: overloadedInstanceFunc2({#x: Int#})[#Int#]{{$}}
// T1-NEXT: SwiftDecl: overloadedInstanceFunc2({#x: Double#})[#Int#]{{$}}
// T1-NEXT: SwiftDecl: curriedVoidFunc1()[#() -> Void#]{{$}}
// T1-NEXT: SwiftDecl: curriedVoidFunc2()[#(a: Int) -> Void#]{{$}}
// T1-NEXT: SwiftDecl: curriedVoidFunc3({#a: Int#})[#() -> Void#]{{$}}
// T1-NEXT: SwiftDecl: curriedVoidFunc4({#a: Int#})[#(b: Int) -> Void#]{{$}}
// T1-NEXT: SwiftDecl: curriedStringFunc1()[#() -> String#]{{$}}
// T1-NEXT: SwiftDecl: curriedStringFunc2()[#(a: Int) -> String#]{{$}}
// T1-NEXT: SwiftDecl: curriedStringFunc3({#a: Int#})[#() -> String#]{{$}}
// T1-NEXT: SwiftDecl: curriedStringFunc4({#a: Int#})[#(b: Int) -> String#]{{$}}
// T1-NEXT: SwiftDecl: selectorVoidFunc1({#Int#}, {#b: Float#})[#Void#]{{$}}
// T1-NEXT: SwiftDecl: selectorVoidFunc2({#Int#}, {#b: Float#}, {#c: Double#})[#Void#]{{$}}
// T1-NEXT: SwiftDecl: selectorStringFunc1({#Int#}, {#b: Float#})[#String#]{{$}}
// T1-NEXT: SwiftDecl: selectorStringFunc2({#Int#}, {#b: Float#}, {#c: Double#})[#String#]{{$}}
// T1-NEXT: SwiftDecl: NestedStruct[#FooStruct.NestedStruct.metatype#]{{$}}
// T1-NEXT: SwiftDecl: NestedClass[#FooStruct.NestedClass.metatype#]{{$}}
// T1-NEXT: SwiftDecl: NestedUnion[#FooStruct.NestedUnion.metatype#]{{$}}
// T1-NEXT: SwiftDecl: extProp[#Int#]{{$}}
// T1-NEXT: SwiftDecl: extFunc0()[#Void#]{{$}}
// T1-NEXT: SwiftDecl: ExtNestedStruct[#ExtNestedStruct.metatype#]{{$}}
// T1-NEXT: SwiftDecl: ExtNestedClass[#ExtNestedClass.metatype#]{{$}}
// T1-NEXT: SwiftDecl: ExtNestedUnion[#ExtNestedUnion.metatype#]{{$}}
// T1-NEXT: Keyword: metatype[#[byref(implicit)] FooStruct.metatype#]{{$}}
// T1-NEXT: End completions
}

func testObjectExprWithoutDot() {
  fooObject#^T2^#
// T2: Begin completions
// T2-NEXT: SwiftDecl: .instanceVar[#Int#]{{$}}
// T2-NEXT: SwiftDecl: .instanceFunc0()[#Void#]{{$}}
// T2-NEXT: SwiftDecl: .instanceFunc1({#a: Int#})[#Void#]{{$}}
// T2-NEXT: SwiftDecl: .instanceFunc2({#a: Int#}, {#b: Double#})[#Void#]{{$}}
//
// FIXME: We lose the '...' sugar on the vararg type.
// Related to rdar://14447454
// T2-NEXT: SwiftDecl: .varargInstanceFunc0({#v: Int[]#})[#Void#]{{$}}
// T2-NEXT: SwiftDecl: .varargInstanceFunc1({#a: Float#}, {#v: Int[]#})[#Void#]{{$}}
// T2-NEXT: SwiftDecl: .varargInstanceFunc2({#a: Float#}, {#b: Double#}, {#v: Int[]#})[#Void#]{{$}}
//
// T2-NEXT: SwiftDecl: .overloadedInstanceFunc1()[#Int#]{{$}}
// T2-NEXT: SwiftDecl: .overloadedInstanceFunc1()[#Double#]{{$}}
// T2-NEXT: SwiftDecl: .overloadedInstanceFunc2({#x: Int#})[#Int#]{{$}}
// T2-NEXT: SwiftDecl: .overloadedInstanceFunc2({#x: Double#})[#Int#]{{$}}
// T2-NEXT: SwiftDecl: [{#i: Int#}][#Double#]{{$}}
// T2-NEXT: SwiftDecl: [{#i: Int#}, {#j: Int#}][#Double#]{{$}}
// T2-NEXT: SwiftDecl: .curriedVoidFunc1()[#() -> Void#]{{$}}
// T2-NEXT: SwiftDecl: .curriedVoidFunc2()[#(a: Int) -> Void#]{{$}}
// T2-NEXT: SwiftDecl: .curriedVoidFunc3({#a: Int#})[#() -> Void#]{{$}}
// T2-NEXT: SwiftDecl: .curriedVoidFunc4({#a: Int#})[#(b: Int) -> Void#]{{$}}
// T2-NEXT: SwiftDecl: .curriedStringFunc1()[#() -> String#]{{$}}
// T2-NEXT: SwiftDecl: .curriedStringFunc2()[#(a: Int) -> String#]{{$}}
// T2-NEXT: SwiftDecl: .curriedStringFunc3({#a: Int#})[#() -> String#]{{$}}
// T2-NEXT: SwiftDecl: .curriedStringFunc4({#a: Int#})[#(b: Int) -> String#]{{$}}
// T2-NEXT: SwiftDecl: .selectorVoidFunc1({#Int#}, {#b: Float#})[#Void#]{{$}}
// T2-NEXT: SwiftDecl: .selectorVoidFunc2({#Int#}, {#b: Float#}, {#c: Double#})[#Void#]{{$}}
// T2-NEXT: SwiftDecl: .selectorStringFunc1({#Int#}, {#b: Float#})[#String#]{{$}}
// T2-NEXT: SwiftDecl: .selectorStringFunc2({#Int#}, {#b: Float#}, {#c: Double#})[#String#]{{$}}
// T2-NEXT: SwiftDecl: .NestedStruct[#FooStruct.NestedStruct.metatype#]{{$}}
// T2-NEXT: SwiftDecl: .NestedClass[#FooStruct.NestedClass.metatype#]{{$}}
// T2-NEXT: SwiftDecl: .NestedUnion[#FooStruct.NestedUnion.metatype#]{{$}}
// T2-NEXT: SwiftDecl: .extProp[#Int#]{{$}}
// T2-NEXT: SwiftDecl: .extFunc0()[#Void#]{{$}}
// T2-NEXT: SwiftDecl: .ExtNestedStruct[#ExtNestedStruct.metatype#]{{$}}
// T2-NEXT: SwiftDecl: .ExtNestedClass[#ExtNestedClass.metatype#]{{$}}
// T2-NEXT: SwiftDecl: .ExtNestedUnion[#ExtNestedUnion.metatype#]{{$}}
// T2-NEXT: Keyword: .metatype[#[byref(implicit)] FooStruct.metatype#]{{$}}
// T2-NEXT: End completions
}

func testMetatypeExpr() {
  FooStruct.#^T3^#
// T3: Begin completions
// T3-NEXT: SwiftDecl: instanceFunc0({#this: [byref] FooStruct#})[#() -> Void#]{{$}}
// T3-NEXT: SwiftDecl: instanceFunc1({#this: [byref] FooStruct#})[#(a: Int) -> Void#]{{$}}
// T3-NEXT: SwiftDecl: instanceFunc2({#this: [byref] FooStruct#})[#(a: Int, b: Double) -> Void#]{{$}}
// T3-NEXT: SwiftDecl: varargInstanceFunc0({#this: [byref] FooStruct#})[#(v: Int[]) -> Void#]{{$}}
// T3-NEXT: SwiftDecl: varargInstanceFunc1({#this: [byref] FooStruct#})[#(a: Float, v: Int[]) -> Void#]{{$}}
// T3-NEXT: SwiftDecl: varargInstanceFunc2({#this: [byref] FooStruct#})[#(a: Float, b: Double, v: Int[]) -> Void#]{{$}}
// T3-NEXT: SwiftDecl: overloadedInstanceFunc1({#this: [byref] FooStruct#})[#() -> Int#]{{$}}
// T3-NEXT: SwiftDecl: overloadedInstanceFunc1({#this: [byref] FooStruct#})[#() -> Double#]{{$}}
// T3-NEXT: SwiftDecl: overloadedInstanceFunc2({#this: [byref] FooStruct#})[#(x: Int) -> Int#]{{$}}
// T3-NEXT: SwiftDecl: overloadedInstanceFunc2({#this: [byref] FooStruct#})[#(x: Double) -> Int#]{{$}}
// T3-NEXT: SwiftDecl: curriedVoidFunc1({#this: [byref] FooStruct#})[#() -> () -> Void#]{{$}}
// T3-NEXT: SwiftDecl: curriedVoidFunc2({#this: [byref] FooStruct#})[#() -> (a: Int) -> Void#]{{$}}
// T3-NEXT: SwiftDecl: curriedVoidFunc3({#this: [byref] FooStruct#})[#(a: Int) -> () -> Void#]{{$}}
// T3-NEXT: SwiftDecl: curriedVoidFunc4({#this: [byref] FooStruct#})[#(a: Int) -> (b: Int) -> Void#]{{$}}
// T3-NEXT: SwiftDecl: curriedStringFunc1({#this: [byref] FooStruct#})[#() -> () -> String#]{{$}}
// T3-NEXT: SwiftDecl: curriedStringFunc2({#this: [byref] FooStruct#})[#() -> (a: Int) -> String#]{{$}}
// T3-NEXT: SwiftDecl: curriedStringFunc3({#this: [byref] FooStruct#})[#(a: Int) -> () -> String#]{{$}}
// T3-NEXT: SwiftDecl: curriedStringFunc4({#this: [byref] FooStruct#})[#(a: Int) -> (b: Int) -> String#]{{$}}
// T3-NEXT: SwiftDecl: selectorVoidFunc1({#this: [byref] FooStruct#})[#(Int, b: Float) -> Void#]{{$}}
// T3-NEXT: SwiftDecl: selectorVoidFunc2({#this: [byref] FooStruct#})[#(Int, b: Float, c: Double) -> Void#]{{$}}
// T3-NEXT: SwiftDecl: selectorStringFunc1({#this: [byref] FooStruct#})[#(Int, b: Float) -> String#]{{$}}
// T3-NEXT: SwiftDecl: selectorStringFunc2({#this: [byref] FooStruct#})[#(Int, b: Float, c: Double) -> String#]{{$}}
// T3-NEXT: SwiftDecl: NestedStruct[#FooStruct.NestedStruct.metatype#]{{$}}
// T3-NEXT: SwiftDecl: NestedClass[#FooStruct.NestedClass.metatype#]{{$}}
// T3-NEXT: SwiftDecl: NestedUnion[#FooStruct.NestedUnion.metatype#]{{$}}
// T3-NEXT: SwiftDecl: NestedTypealias[#Int.metatype#]{{$}}
// T3-NEXT: SwiftDecl: staticFunc0()[#Void#]{{$}}
// T3-NEXT: SwiftDecl: staticFunc1({#a: Int#})[#Void#]{{$}}
// T3-NEXT: SwiftDecl: overloadedStaticFunc1()[#Int#]{{$}}
// T3-NEXT: SwiftDecl: overloadedStaticFunc1()[#Double#]{{$}}
// T3-NEXT: SwiftDecl: overloadedStaticFunc2({#x: Int#})[#Int#]{{$}}
// T3-NEXT: SwiftDecl: overloadedStaticFunc2({#x: Double#})[#Int#]{{$}}
// T3-NEXT: SwiftDecl: extFunc0({#this: [byref] FooStruct#})[#() -> Void#]{{$}}
// T3-NEXT: SwiftDecl: extStaticFunc0()[#Void#]{{$}}
// T3-NEXT: SwiftDecl: ExtNestedStruct[#ExtNestedStruct.metatype#]{{$}}
// T3-NEXT: SwiftDecl: ExtNestedClass[#ExtNestedClass.metatype#]{{$}}
// T3-NEXT: SwiftDecl: ExtNestedUnion[#ExtNestedUnion.metatype#]{{$}}
// T3-NEXT: SwiftDecl: ExtNestedTypealias[#Int.metatype#]{{$}}
// T3-NEXT: Keyword: metatype[#FooStruct.metatype.metatype#]{{$}}
// T3-NEXT: End completions
}

func testMetatypeExprWithoutDot() {
  FooStruct#^T4^#
// T4: Begin completions
// T4-NEXT: SwiftDecl: .instanceFunc0({#this: [byref] FooStruct#})[#() -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .instanceFunc1({#this: [byref] FooStruct#})[#(a: Int) -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .instanceFunc2({#this: [byref] FooStruct#})[#(a: Int, b: Double) -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .varargInstanceFunc0({#this: [byref] FooStruct#})[#(v: Int[]) -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .varargInstanceFunc1({#this: [byref] FooStruct#})[#(a: Float, v: Int[]) -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .varargInstanceFunc2({#this: [byref] FooStruct#})[#(a: Float, b: Double, v: Int[]) -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .overloadedInstanceFunc1({#this: [byref] FooStruct#})[#() -> Int#]{{$}}
// T4-NEXT: SwiftDecl: .overloadedInstanceFunc1({#this: [byref] FooStruct#})[#() -> Double#]{{$}}
// T4-NEXT: SwiftDecl: .overloadedInstanceFunc2({#this: [byref] FooStruct#})[#(x: Int) -> Int#]{{$}}
// T4-NEXT: SwiftDecl: .overloadedInstanceFunc2({#this: [byref] FooStruct#})[#(x: Double) -> Int#]{{$}}
// T4-NEXT: SwiftDecl: .curriedVoidFunc1({#this: [byref] FooStruct#})[#() -> () -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .curriedVoidFunc2({#this: [byref] FooStruct#})[#() -> (a: Int) -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .curriedVoidFunc3({#this: [byref] FooStruct#})[#(a: Int) -> () -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .curriedVoidFunc4({#this: [byref] FooStruct#})[#(a: Int) -> (b: Int) -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .curriedStringFunc1({#this: [byref] FooStruct#})[#() -> () -> String#]{{$}}
// T4-NEXT: SwiftDecl: .curriedStringFunc2({#this: [byref] FooStruct#})[#() -> (a: Int) -> String#]{{$}}
// T4-NEXT: SwiftDecl: .curriedStringFunc3({#this: [byref] FooStruct#})[#(a: Int) -> () -> String#]{{$}}
// T4-NEXT: SwiftDecl: .curriedStringFunc4({#this: [byref] FooStruct#})[#(a: Int) -> (b: Int) -> String#]{{$}}
// T4-NEXT: SwiftDecl: .selectorVoidFunc1({#this: [byref] FooStruct#})[#(Int, b: Float) -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .selectorVoidFunc2({#this: [byref] FooStruct#})[#(Int, b: Float, c: Double) -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .selectorStringFunc1({#this: [byref] FooStruct#})[#(Int, b: Float) -> String#]{{$}}
// T4-NEXT: SwiftDecl: .selectorStringFunc2({#this: [byref] FooStruct#})[#(Int, b: Float, c: Double) -> String#]{{$}}
// T4-NEXT: SwiftDecl: .NestedStruct[#FooStruct.NestedStruct.metatype#]{{$}}
// T4-NEXT: SwiftDecl: .NestedClass[#FooStruct.NestedClass.metatype#]{{$}}
// T4-NEXT: SwiftDecl: .NestedUnion[#FooStruct.NestedUnion.metatype#]{{$}}
// T4-NEXT: SwiftDecl: .NestedTypealias[#Int.metatype#]{{$}}
// T4-NEXT: SwiftDecl: .staticFunc0()[#Void#]{{$}}
// T4-NEXT: SwiftDecl: .staticFunc1({#a: Int#})[#Void#]{{$}}
// T4-NEXT: SwiftDecl: .overloadedStaticFunc1()[#Int#]{{$}}
// T4-NEXT: SwiftDecl: .overloadedStaticFunc1()[#Double#]{{$}}
// T4-NEXT: SwiftDecl: .overloadedStaticFunc2({#x: Int#})[#Int#]{{$}}
// T4-NEXT: SwiftDecl: .overloadedStaticFunc2({#x: Double#})[#Int#]{{$}}
// T4-NEXT: SwiftDecl: ({#instanceVar: Int#})[#FooStruct#]{{$}}
// T4-NEXT: SwiftDecl: ()[#FooStruct#]{{$}}
// T4-NEXT: SwiftDecl: .extFunc0({#this: [byref] FooStruct#})[#() -> Void#]{{$}}
// T4-NEXT: SwiftDecl: .extStaticFunc0()[#Void#]{{$}}
// T4-NEXT: SwiftDecl: .ExtNestedStruct[#ExtNestedStruct.metatype#]{{$}}
// T4-NEXT: SwiftDecl: .ExtNestedClass[#ExtNestedClass.metatype#]{{$}}
// T4-NEXT: SwiftDecl: .ExtNestedUnion[#ExtNestedUnion.metatype#]{{$}}
// T4-NEXT: SwiftDecl: .ExtNestedTypealias[#Int.metatype#]{{$}}
// T4-NEXT: Keyword: .metatype[#FooStruct.metatype.metatype#]{{$}}
// T4-NEXT: End completions
}

func testObjectExprWithoutSpaceAfterCodeCompletion() {
  fooObject#^T5^#.bar
// T5: Begin completions
// T5-DAG: SwiftDecl: .instanceVar[#Int#]{{$}}
// T5: End completions
}

func testDotDotTokenSplitWithCodeCompletion() {
  fooObject.#^DOT_DOT_TOKEN_SPLIT^#.bar
// DOT_DOT_TOKEN_SPLIT: Begin completions
// DOT_DOT_TOKEN_SPLIT-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// DOT_DOT_TOKEN_SPLIT: End completions
}

func testCurriedFunc() {
  fooObject.curriedVoidFunc1()#^CF1^#
// CF1: Begin completions
// CF1-NEXT: Pattern: ()[#Void#]{{$}}
// CF1-NEXT: Keyword: .metatype[#() -> ().metatype#]{{$}}
// CF1-NEXT: End completions

  fooObject.curriedVoidFunc2()#^CF2^#
// CF2: Begin completions
// CF2-NEXT: Pattern: ({#a: Int#})[#Void#]{{$}}
// CF2-NEXT: Keyword: .metatype[#(a : Int64) -> ().metatype#]{{$}}
// CF2-NEXT: End completions

  fooObject.curriedVoidFunc3(42)#^CF3^#
// CF3: Begin completions
// CF3-NEXT: Pattern: ()[#Void#]{{$}}
// CF3-NEXT: Keyword: .metatype[#() -> ().metatype#]{{$}}
// CF3-NEXT: End completions

  fooObject.curriedVoidFunc4(42)#^CF4^#
// CF4: Begin completions
// CF4-NEXT: Pattern: ({#b: Int#})[#Void#]{{$}}
// CF4-NEXT: Keyword: .metatype[#(b : Int64) -> ().metatype#]{{$}}
// CF4-NEXT: End completions
}

func testImplicitlyCurriedFunc() {
  FooStruct.instanceFunc0(FooStruct())#^IMPLICITLY_CURRIED_FUNC_0^#
// IMPLICITLY_CURRIED_FUNC_0: Begin completions
// IMPLICITLY_CURRIED_FUNC_0-NEXT: Pattern: ()[#Void#]{{$}}
// IMPLICITLY_CURRIED_FUNC_0-NEXT: Keyword: .metatype[#() -> ().metatype#]{{$}}
// IMPLICITLY_CURRIED_FUNC_0-NEXT: End completions

  FooStruct.instanceFunc1(FooStruct())#^IMPLICITLY_CURRIED_FUNC_1^#
// IMPLICITLY_CURRIED_FUNC_1: Begin completions
// IMPLICITLY_CURRIED_FUNC_1-NEXT: Pattern: ({#a: Int#})[#Void#]{{$}}
// IMPLICITLY_CURRIED_FUNC_1-NEXT: Keyword: .metatype[#(a : Int64) -> ().metatype#]{{$}}
// IMPLICITLY_CURRIED_FUNC_1-NEXT: End completions

  FooStruct.instanceFunc2(FooStruct())#^IMPLICITLY_CURRIED_FUNC_2^#
// IMPLICITLY_CURRIED_FUNC_2: Begin completions
// IMPLICITLY_CURRIED_FUNC_2-NEXT: Pattern: ({#a: Int#}, {#b: Double#})[#Void#]{{$}}
// IMPLICITLY_CURRIED_FUNC_2-NEXT: Keyword: .metatype[#(a : Int64, b : Float64) -> ().metatype#]{{$}}
// IMPLICITLY_CURRIED_FUNC_2-NEXT: End completions

  FooStruct.varargInstanceFunc0(FooStruct())#^IMPLICITLY_CURRIED_VARARG_FUNC_0^#
// IMPLICITLY_CURRIED_VARARG_FUNC_0: Begin completions
// IMPLICITLY_CURRIED_VARARG_FUNC_0-NEXT: Pattern: ({#v: Int[]#})[#Void#]{{$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_0-NEXT: Keyword: .metatype[#(v : Int64...) -> ().metatype#]{{$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_0-NEXT: End completions

  FooStruct.varargInstanceFunc1(FooStruct())#^IMPLICITLY_CURRIED_VARARG_FUNC_1^#
// IMPLICITLY_CURRIED_VARARG_FUNC_1: Begin completions
// IMPLICITLY_CURRIED_VARARG_FUNC_1-NEXT: Pattern: ({#a: Float#}, {#v: Int[]#})[#Void#]{{$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_1-NEXT: Keyword: .metatype[#(a : Float32, v : Int64...) -> ().metatype#]{{$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_1-NEXT: End completions

  FooStruct.varargInstanceFunc2(FooStruct())#^IMPLICITLY_CURRIED_VARARG_FUNC_2^#
// IMPLICITLY_CURRIED_VARARG_FUNC_2: Begin completions
// IMPLICITLY_CURRIED_VARARG_FUNC_2-NEXT: Pattern: ({#a: Float#}, {#b: Double#}, {#v: Int[]#})[#Void#]{{$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_2-NEXT: Keyword: .metatype[#(a : Float32, b : Float64, v : Int64...) -> ().metatype#]{{$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_2-NEXT: End completions

  // This call is ambiguous, and the expression is invalid.
  // Ensure that we don't suggest to call the result.
  FooStruct.overloadedInstanceFunc1(FooStruct())#^IMPLICITLY_CURRIED_OVERLOADED_FUNC_1^#
// IMPLICITLY_CURRIED_OVERLOADED_FUNC_1: found code completion token
// IMPLICITLY_CURRIED_OVERLOADED_FUNC_1-NOT: Begin completions

  // This call is ambiguous, and the expression is invalid.
  // Ensure that we don't suggest to call the result.
  FooStruct.overloadedInstanceFunc2(FooStruct())#^IMPLICITLY_CURRIED_OVERLOADED_FUNC_2^#
// IMPLICITLY_CURRIED_OVERLOADED_FUNC_2: found code completion token
// IMPLICITLY_CURRIED_OVERLOADED_FUNC_2-NOT: Begin completions

  FooStruct.curriedVoidFunc1(FooStruct())#^IMPLICITLY_CURRIED_CURRIED_FUNC_1^#
// IMPLICITLY_CURRIED_CURRIED_FUNC_1: Begin completions
// IMPLICITLY_CURRIED_CURRIED_FUNC_1-NEXT: Pattern: ()[#() -> ()#]{{$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_1-NEXT: Keyword: .metatype[#() -> () -> ().metatype#]{{$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_1-NEXT: End completions

  FooStruct.curriedVoidFunc2(FooStruct())#^IMPLICITLY_CURRIED_CURRIED_FUNC_2^#
// IMPLICITLY_CURRIED_CURRIED_FUNC_2: Begin completions
// IMPLICITLY_CURRIED_CURRIED_FUNC_2-NEXT: Pattern: ()[#(a : Int) -> ()#]{{$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_2-NEXT: Keyword: .metatype[#() -> (a : Int64) -> ().metatype#]{{$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_2-NEXT: End completions

  FooStruct.curriedVoidFunc3(FooStruct())#^IMPLICITLY_CURRIED_CURRIED_FUNC_3^#
// IMPLICITLY_CURRIED_CURRIED_FUNC_3: Begin completions
// IMPLICITLY_CURRIED_CURRIED_FUNC_3-NEXT: Pattern: ({#a: Int#})[#() -> ()#]{{$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_3-NEXT: Keyword: .metatype[#(a : Int64) -> () -> ().metatype#]{{$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_3-NEXT: End completions

  FooStruct.curriedVoidFunc4(FooStruct())#^IMPLICITLY_CURRIED_CURRIED_FUNC_4^#
// IMPLICITLY_CURRIED_CURRIED_FUNC_4: Begin completions
// IMPLICITLY_CURRIED_CURRIED_FUNC_4-NEXT: Pattern: ({#a: Int#})[#(b : Int) -> ()#]{{$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_4-NEXT: Keyword: .metatype[#(a : Int64) -> (b : Int64) -> ().metatype#]{{$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_4-NEXT: End completions
}

//===---
// Test that we can code complete in methods, and correctly distinguish static
// and non-static contexts.
//===---

struct CodeCompletionInMethods {
  /// @{ Members.
  /// Warning: there are negative tests about code completion of instance
  /// members of this class.  Read the tests below before adding, removing or
  /// modifying members.

  var instanceVar : Int

  func instanceFunc0() {}
  func instanceFunc1(a : Int) {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    instanceVar = i
  }

  struct NestedStruct {}
  class NestedClass {}
  union NestedUnion {}
  // Can not declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int

  // FIXME: uncomment when we have static vars.
  // static var staticVar : Int

  static func staticFunc0() {}
  static func staticFunc1(a : Int) {}

  /// @} Members.

  /// @{ Tests.

  func instanceTest1() {
    #^IN_INSTANCE_METHOD_1^#
// IN_INSTANCE_METHOD_1: Begin completions
// IN_INSTANCE_METHOD_1-DAG: SwiftDecl: this[#[byref] CodeCompletionInMethods#]{{$}}
// IN_INSTANCE_METHOD_1-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// IN_INSTANCE_METHOD_1-DAG: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// IN_INSTANCE_METHOD_1-DAG: SwiftDecl: instanceFunc1({#a: Int#})[#Void#]{{$}}
// IN_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedStruct[#CodeCompletionInMethods.NestedStruct.metatype#]{{$}}
// IN_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedClass[#CodeCompletionInMethods.NestedClass.metatype#]{{$}}
// IN_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedUnion[#CodeCompletionInMethods.NestedUnion.metatype#]{{$}}
// IN_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedTypealias[#Int.metatype#]{{$}}
// IN_INSTANCE_METHOD_1: End completions
  }

  static func staticTest1() {
    #^IN_STATIC_METHOD_1^#
// Negative tests.
// IN_STATIC_METHOD_1_NEGATIVE-NOT: SwiftDecl: instanceVar
//
// Positive tests.
// IN_STATIC_METHOD_1: Begin completions
// IN_STATIC_METHOD_1-DAG: SwiftDecl: instanceFunc0({#this: [byref] CodeCompletionInMethods#})[#() -> Void#]{{$}}
// IN_STATIC_METHOD_1-DAG: SwiftDecl: instanceFunc1({#this: [byref] CodeCompletionInMethods#})[#(a: Int) -> Void#]{{$}}
// IN_STATIC_METHOD_1-DAG: SwiftDecl: this[#CodeCompletionInMethods.metatype#]{{$}}
// IN_STATIC_METHOD_1-DAG: SwiftDecl: NestedStruct[#CodeCompletionInMethods.NestedStruct.metatype#]{{$}}
// IN_STATIC_METHOD_1-DAG: SwiftDecl: NestedClass[#CodeCompletionInMethods.NestedClass.metatype#]{{$}}
// IN_STATIC_METHOD_1-DAG: SwiftDecl: NestedUnion[#CodeCompletionInMethods.NestedUnion.metatype#]{{$}}
// IN_STATIC_METHOD_1-DAG: SwiftDecl: NestedTypealias[#Int.metatype#]{{$}}
// IN_STATIC_METHOD_1-DAG: SwiftDecl: staticFunc0()[#Void#]{{$}}
// IN_STATIC_METHOD_1-DAG: SwiftDecl: staticFunc1({#a: Int#})[#Void#]{{$}}
// IN_STATIC_METHOD_1: End completions
  }

  // FIXME: Add constructor and destructor tests when we can code complete
  // there.
  // constructor() {}
  // destructor() {}

  /// @}
}

//===--- Helper types that are used in this test

struct FooGenericStruct<T> {
  var fooInstanceVarT : T
  func fooVoidInstanceFunc1(a : T) {}
  func fooTInstanceFunc1(a : T) -> T { return a }
}

class FooClass {
  var fooClassInstanceVar : Int
  func fooClassInstanceFunc0() {}
}

union FooUnion {
}

protocol FooProtocol {
  var fooInstanceVar : Int
  typealias FooTypeAlias1
  func fooInstanceFunc0() -> Double
  func fooInstanceFunc1(a : Int) -> Double
}

class FooProtocolImpl : FooProtocol {
  var fooInstanceVar : Int
  typealias FooTypeAlias1 = Float
  func fooInstanceFunc0() -> Double {
    return 0.0
  }
  func fooInstanceFunc1(a : Int) -> Double {
    return Double(a)
  }
}

protocol FooExProtocol : FooProtocol {
  func fooExInstanceFunc0() -> Double
}

protocol BarProtocol {
  var barInstanceVar : Int
  typealias BarTypeAlias1
  func barInstanceFunc0() -> Double
  func barInstanceFunc1(a : Int) -> Double
}

protocol BarExProtocol : BarProtocol {
  func barExInstanceFunc0() -> Double
}

protocol BazProtocol {
  func bazInstanceFunc0() -> Double
}

typealias BarBazProtocolComposition = protocol<BarProtocol, BazProtocol>

var fooProtocolInstance : FooProtocol = FooProtocolImpl()
var fooBarProtocolInstance : protocol<FooProtocol, BarProtocol>
var fooExBarExProtocolInstance : protocol<FooExProtocol, BarExProtocol>

typealias FooTypealias = Int

func testExprPostfixBeginA() {
  #^EXPR_POSTFIX_BEGIN_1^#
// EXPR_POSTFIX_BEGIN_1: Begin completions
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: FooStruct[#FooStruct.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: FooUnion[#FooUnion.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: FooClass[#FooClass.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: FooProtocol[#FooProtocol.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: FooTypealias[#Int.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: true[#Bool#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: false[#Bool#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Int8[#Int8.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Int16[#Int16.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Int32[#Int32.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Int64[#Int64.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Int128[#Int128.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Bool[#Bool.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1: End completions

  1 + #^EXPR_POSTFIX_BEGIN_2^#
// EXPR_POSTFIX_BEGIN_2: Begin completions
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: FooStruct[#FooStruct.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: FooUnion[#FooUnion.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: FooClass[#FooClass.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: FooProtocol[#FooProtocol.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: FooTypealias[#Int.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: true[#Bool#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: false[#Bool#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Int8[#Int8.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Int16[#Int16.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Int32[#Int32.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Int64[#Int64.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Int128[#Int128.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Bool[#Bool.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2: End completions
}

func testExprPostfixBeginB(a : Int, b : Float)(c : Double) {
  #^EXPR_POSTFIX_BEGIN_3^#
// EXPR_POSTFIX_BEGIN_3: Begin completions
// EXPR_POSTFIX_BEGIN_3-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_3-DAG: SwiftDecl: b[#Float#]{{$}}
// EXPR_POSTFIX_BEGIN_3-DAG: SwiftDecl: c[#Double#]{{$}}
// EXPR_POSTFIX_BEGIN_3: End completions
}

func testExprPostfixBeginC<Foo : FooProtocol>(foo : Foo) {
  #^EXPR_POSTFIX_BEGIN_4^#
// EXPR_POSTFIX_BEGIN_4: Begin completions
// EXPR_POSTFIX_BEGIN_4-DAG: SwiftDecl: Foo[#Foo.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_4-DAG: SwiftDecl: foo[#Foo#]{{$}}
// EXPR_POSTFIX_BEGIN_4: End completions
}

struct TestExprPostfixBeginD {
  func testExprPostfixBeginE(a : Int, b : Float)(c : Double) {
    #^EXPR_POSTFIX_BEGIN_5^#
// EXPR_POSTFIX_BEGIN_5: Begin completions
// EXPR_POSTFIX_BEGIN_5-DAG: SwiftDecl: this[#[byref] TestExprPostfixBeginD#]{{$}}
// EXPR_POSTFIX_BEGIN_5-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_5-DAG: SwiftDecl: b[#Float#]{{$}}
// EXPR_POSTFIX_BEGIN_5-DAG: SwiftDecl: c[#Double#]{{$}}
// EXPR_POSTFIX_BEGIN_5: End completions
  }

  func testExprPostfixBeginF<U>(a : Int, b : U) {
    #^EXPR_POSTFIX_BEGIN_6^#
// EXPR_POSTFIX_BEGIN_6: Begin completions
// EXPR_POSTFIX_BEGIN_6-DAG: SwiftDecl: U[#U.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_6-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_6-DAG: SwiftDecl: b[#U#]{{$}}
// EXPR_POSTFIX_BEGIN_6: End completions
  }
}

struct TestExprPostfixBeginG<T> {
  func testExprPostfixBeginH(a : Int, b : T) {
    #^EXPR_POSTFIX_BEGIN_7^#
// EXPR_POSTFIX_BEGIN_7: Begin completions
// EXPR_POSTFIX_BEGIN_7-DAG: SwiftDecl: T[#T.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_7-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_7-DAG: SwiftDecl: b[#T#]{{$}}
// EXPR_POSTFIX_BEGIN_7: End completions
  }

  func testExprPostfixBeginI<U>(a : Int, b : T, c : U) {
    #^EXPR_POSTFIX_BEGIN_8^#
// EXPR_POSTFIX_BEGIN_8: Begin completions
// EXPR_POSTFIX_BEGIN_8-DAG: SwiftDecl: T[#T.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_8-DAG: SwiftDecl: U[#U.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_8-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_8-DAG: SwiftDecl: b[#T#]{{$}}
// EXPR_POSTFIX_BEGIN_8-DAG: SwiftDecl: c[#U#]{{$}}
// EXPR_POSTFIX_BEGIN_8: End completions
  }
}

class TestExprPostfixBeginJ {
  func testExprPostfixBeginK(a : Int, b : Float)(c : Double) {
    #^EXPR_POSTFIX_BEGIN_9^#
// EXPR_POSTFIX_BEGIN_9: Begin completions
// EXPR_POSTFIX_BEGIN_9-DAG: SwiftDecl: this[#TestExprPostfixBeginJ#]{{$}}
// EXPR_POSTFIX_BEGIN_9-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_9-DAG: SwiftDecl: b[#Float#]{{$}}
// EXPR_POSTFIX_BEGIN_9-DAG: SwiftDecl: c[#Double#]{{$}}
// EXPR_POSTFIX_BEGIN_9: End completions
  }
}

//===--- Test that we can code complete inside function calls.

func testInsideFunctionCall1() {
  FooStruct().instanceFunc0(#^INSIDE_FUNCTION_CALL_1^#
// INSIDE_FUNCTION_CALL_1: Begin completions
// FIXME: There should be no results here because the function call
// unambigously resolves to overload that takes 0 arguments.
// INSIDE_FUNCTION_CALL_1-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// INSIDE_FUNCTION_CALL_1: End completions
}

func testInsideFunctionCall2() {
  FooStruct().instanceFunc1(#^INSIDE_FUNCTION_CALL_2^#
// INSIDE_FUNCTION_CALL_2: Begin completions
// INSIDE_FUNCTION_CALL_2-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// INSIDE_FUNCTION_CALL_2: End completions
}

func testInsideFunctionCall3() {
  FooStruct().instanceFunc1(42, #^INSIDE_FUNCTION_CALL_3^#
// INSIDE_FUNCTION_CALL_3: Begin completions
// FIXME: There should be no results here because the function call
// unambigously resolves to overload that takes 1 argument.
// INSIDE_FUNCTION_CALL_3-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// INSIDE_FUNCTION_CALL_3: End completions
}

func testInsideFunctionCall4() {
  FooStruct().instanceFunc2(#^INSIDE_FUNCTION_CALL_4^#
// INSIDE_FUNCTION_CALL_4: Begin completions
// INSIDE_FUNCTION_CALL_4-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// INSIDE_FUNCTION_CALL_4: End completions
}

func testInsideFunctionCall5() {
  FooStruct().instanceFunc2(42, #^INSIDE_FUNCTION_CALL_5^#
// INSIDE_FUNCTION_CALL_5: Begin completions
// INSIDE_FUNCTION_CALL_5-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// INSIDE_FUNCTION_CALL_5: End completions
}

func testInsideVarargFunctionCall1() {
  FooStruct().varargInstanceFunc0(#^INSIDE_VARARG_FUNCTION_CALL_1^#
// INSIDE_VARARG_FUNCTION_CALL_1: Begin completions
// INSIDE_VARARG_FUNCTION_CALL_1-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// INSIDE_VARARG_FUNCTION_CALL_1: End completions
}

func testInsideVarargFunctionCall2() {
  FooStruct().varargInstanceFunc0(42, #^INSIDE_VARARG_FUNCTION_CALL_2^#
// INSIDE_VARARG_FUNCTION_CALL_2: Begin completions
// INSIDE_VARARG_FUNCTION_CALL_2-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// INSIDE_VARARG_FUNCTION_CALL_2: End completions
}

func testInsideVarargFunctionCall3() {
  FooStruct().varargInstanceFunc0(42, 4242, #^INSIDE_VARARG_FUNCTION_CALL_3^#
// INSIDE_VARARG_FUNCTION_CALL_3: Begin completions
// INSIDE_VARARG_FUNCTION_CALL_3-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// INSIDE_VARARG_FUNCTION_CALL_3: End completions
}

//===--- Variables that have function types.

class FuncTypeVars {
  var funcVar1 : () -> Double
  var funcVar2 : (a : Int) -> Double
}

var funcTypeVarsObject : FuncTypeVars
func testFuncTypeVars() {
  funcTypeVarsObject.funcVar1#^VF1^#
// VF1: Begin completions
// VF1-NEXT: Pattern: ()[#Double#]{{$}}
// VF1-NEXT: Keyword: .metatype[#[byref] () -> Float64.metatype#]{{$}}
// VF1-NEXT: End completions

  funcTypeVarsObject.funcVar2#^VF2^#
// VF2: Begin completions
// VF2-NEXT: Pattern: ({#a: Int#})[#Double#]{{$}}
// VF2-NEXT: Keyword: .metatype[#[byref] (a : Int64) -> Float64.metatype#]{{$}}
// VF2-NEXT: End completions
}

//===--- Check that we look into base classes.

class MembersBase {
  var baseVar : Int
  func baseInstanceFunc() {}
  static func baseStaticFunc() {}
}

class MembersDerived : MembersBase {
  var derivedVar : Int
  func derivedInstanceFunc() {}
  static func derivedStaticFunc() {}
}

var membersDerived : MembersDerived
func testLookInBase() {
  membersDerived.#^BASE_MEMBERS^#
// BASE_MEMBERS: Begin completions
// BASE_MEMBERS-NEXT: SwiftDecl: derivedVar[#Int#]{{$}}
// BASE_MEMBERS-NEXT: SwiftDecl: derivedInstanceFunc()[#Void#]{{$}}
// BASE_MEMBERS-NEXT: SwiftDecl: baseVar[#Int#]{{$}}
// BASE_MEMBERS-NEXT: SwiftDecl: baseInstanceFunc()[#Void#]{{$}}
// BASE_MEMBERS-NEXT: Keyword: metatype[#[byref(implicit)] MembersDerived.metatype#]{{$}}
// BASE_MEMBERS-NEXT: End completions
}

func testLookInBaseStatic() {
  MembersDerived.#^BASE_MEMBERS_STATIC^#
// BASE_MEMBERS_STATIC: Begin completions
// BASE_MEMBERS_STATIC-NEXT: SwiftDecl: derivedInstanceFunc({#this: MembersDerived#})[#() -> Void#]{{$}}
// BASE_MEMBERS_STATIC-NEXT: SwiftDecl: derivedStaticFunc()[#Void#]{{$}}
// BASE_MEMBERS_STATIC-NEXT: SwiftDecl: baseInstanceFunc({#this: MembersBase#})[#() -> Void#]{{$}}
// BASE_MEMBERS_STATIC-NEXT: SwiftDecl: baseStaticFunc()[#Void#]{{$}}
// BASE_MEMBERS_STATIC-NEXT: Keyword: metatype[#MembersDerived.metatype.metatype#]{{$}}
// BASE_MEMBERS_STATIC-NEXT: End completions
}

//===--- Check that we can look into protocols.

func testLookInProto1() {
  fooProtocolInstance.#^PROTO_MEMBERS_1^#
// PROTO_MEMBERS_1: Begin completions
// PROTO_MEMBERS_1-NEXT: SwiftDecl: fooInstanceVar[#Int#]{{$}}
// PROTO_MEMBERS_1-NEXT: SwiftDecl: fooInstanceFunc0()[#Double#]{{$}}
// PROTO_MEMBERS_1-NEXT: SwiftDecl: fooInstanceFunc1({#a: Int#})[#Double#]{{$}}
// PROTO_MEMBERS_1-NEXT: Keyword: metatype[#[byref(implicit)] FooProtocol.metatype#]{{$}}
// PROTO_MEMBERS_1-NEXT: End completions
}

func testLookInProto2() {
  fooBarProtocolInstance.#^PROTO_MEMBERS_2^#
// PROTO_MEMBERS_2: Begin completions
// PROTO_MEMBERS_2-NEXT: SwiftDecl: barInstanceVar[#Int#]{{$}}
// PROTO_MEMBERS_2-NEXT: SwiftDecl: barInstanceFunc0()[#Double#]{{$}}
// PROTO_MEMBERS_2-NEXT: SwiftDecl: barInstanceFunc1({#a: Int#})[#Double#]{{$}}
// PROTO_MEMBERS_2-NEXT: SwiftDecl: fooInstanceVar[#Int#]{{$}}
// PROTO_MEMBERS_2-NEXT: SwiftDecl: fooInstanceFunc0()[#Double#]{{$}}
// PROTO_MEMBERS_2-NEXT: SwiftDecl: fooInstanceFunc1({#a: Int#})[#Double#]{{$}}
// PROTO_MEMBERS_2-NEXT: Keyword: metatype[#[byref(implicit)] protocol<BarProtocol, FooProtocol>.metatype#]{{$}}
// PROTO_MEMBERS_2-NEXT: End completions
}

func testLookInProto3() {
  fooExBarExProtocolInstance.#^PROTO_MEMBERS_3^#
// PROTO_MEMBERS_3: Begin completions
// PROTO_MEMBERS_3-NEXT: SwiftDecl: barInstanceVar[#Int#]{{$}}
// PROTO_MEMBERS_3-NEXT: SwiftDecl: barInstanceFunc0()[#Double#]{{$}}
// PROTO_MEMBERS_3-NEXT: SwiftDecl: barInstanceFunc1({#a: Int#})[#Double#]{{$}}
// PROTO_MEMBERS_3-NEXT: SwiftDecl: barExInstanceFunc0()[#Double#]{{$}}
// PROTO_MEMBERS_3-NEXT: SwiftDecl: fooInstanceVar[#Int#]{{$}}
// PROTO_MEMBERS_3-NEXT: SwiftDecl: fooInstanceFunc0()[#Double#]{{$}}
// PROTO_MEMBERS_3-NEXT: SwiftDecl: fooInstanceFunc1({#a: Int#})[#Double#]{{$}}
// PROTO_MEMBERS_3-NEXT: SwiftDecl: fooExInstanceFunc0()[#Double#]{{$}}
// PROTO_MEMBERS_3-NEXT: Keyword: metatype[#[byref(implicit)] protocol<BarExProtocol, FooExProtocol>.metatype#]{{$}}
// PROTO_MEMBERS_3-NEXT: End completions
}

func testLookInProto4(a: protocol<FooProtocol, BarBazProtocolComposition>) {
  a.#^PROTO_MEMBERS_4^#
// PROTO_MEMBERS_4: Begin completions
// PROTO_MEMBERS_4-DAG: SwiftDecl: fooInstanceFunc0()[#Double#]{{$}}
// PROTO_MEMBERS_4-DAG: SwiftDecl: barInstanceFunc0()[#Double#]{{$}}
// PROTO_MEMBERS_4-DAG: SwiftDecl: bazInstanceFunc0()[#Double#]{{$}}
// PROTO_MEMBERS_4: End completions
}

//===--- Tests for 'super'.

class SuperBase1 {
  var baseInstanceVar: Int

  var baseProp : Int {
  get:
    return 42
  set(val):
  }

  // Don't declare constructors.

  func baseFunc0() {}
  func baseFunc1(a: Int) {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    baseInstanceVar = i
  }

  struct BaseNestedStruct {}
  class BaseNestedClass {}
  union BaseNestedUnion {
    case BaseUnionX(Int)
  }

  typealias BaseNestedTypealias = Int
}

extension SuperBase1 {
  var baseExtProp : Int {
  get:
    return 42
  set(val):
  }

  func baseExtFunc0() {}

  static func baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  union BaseExtNestedUnion {
    case BaseExtUnionX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

class SuperDerived1 : SuperBase1 {
  var derivedInstanceVar: Int

  constructor() {
    // FIXME: Disabled because we don't delay constructors' bodies.
    // Results should include calls to base constructors that are implicitly defined.
    super#^SUPER_NO_DOT_1^#
  }

  func derivedFunc0() {}

  func test1() {
    super#^SUPER_NO_DOT_2^#
// SUPER_NO_DOT_2: Begin completions
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseInstanceVar[#Int#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseProp[#Int#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseFunc0()[#Void#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseFunc1({#a: Int#})[#Void#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: [{#i: Int#}][#Double#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseNestedStruct[#SuperBase1.BaseNestedStruct.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseNestedClass[#SuperBase1.BaseNestedClass.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseNestedUnion[#SuperBase1.BaseNestedUnion.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseExtProp[#Int#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseExtFunc0()[#Void#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseExtNestedStruct[#BaseExtNestedStruct.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseExtNestedClass[#BaseExtNestedClass.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseExtNestedUnion[#BaseExtNestedUnion.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: Keyword: .metatype[#[byref] SuperBase1.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: End completions
  }

  func test2() {
    super.#^SUPER_DOT_1^#
// SUPER_DOT_1: Begin completions
// SUPER_DOT_1-NEXT: SwiftDecl: baseInstanceVar[#Int#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: baseProp[#Int#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: baseFunc0()[#Void#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: baseFunc1({#a: Int#})[#Void#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseNestedStruct[#SuperBase1.BaseNestedStruct.metatype#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseNestedClass[#SuperBase1.BaseNestedClass.metatype#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseNestedUnion[#SuperBase1.BaseNestedUnion.metatype#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: baseExtProp[#Int#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: baseExtFunc0()[#Void#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseExtNestedStruct[#BaseExtNestedStruct.metatype#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseExtNestedClass[#BaseExtNestedClass.metatype#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseExtNestedUnion[#BaseExtNestedUnion.metatype#]{{$}}
// SUPER_DOT_1-NEXT: Keyword: metatype[#[byref] SuperBase1.metatype#]{{$}}
// SUPER_DOT_1-NEXT: End completions
  }
}

class SuperBase2 {
  var baseInstanceVar: Int

  var baseProp : Int {
  get:
    return 42
  set(val):
  }

  constructor() {}
  constructor(a: Double) {}

  func baseFunc0() {}
  func baseFunc1(a: Int) {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    baseInstanceVar = i
  }

  struct BaseNestedStruct {}
  class BaseNestedClass {}
  union BaseNestedUnion {
    case BaseUnionX(Int)
  }

  typealias BaseNestedTypealias = Int
}

extension SuperBase2 {
  var baseExtProp : Int {
  get:
    return 42
  set(val):
  }

  func baseExtFunc0() {}

  static func baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  union BaseExtNestedUnion {
    case BaseExtUnionX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

class SuperDerived2 : SuperBase2 {
  var derivedInstanceVar: Int

  constructor() {
    // FIXME: Disabled because we don't delay constructors' bodies.
    // Results should include calls to base constructors that are explicitly defined.
    super#^SUPER_NO_DOT_3^#
  }

  func derivedFunc0() {}

  func test1() {
    super#^SUPER_NO_DOT_4^#
// SUPER_NO_DOT_4: Begin completions
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseInstanceVar[#Int#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseProp[#Int#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseFunc0()[#Void#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseFunc1({#a: Int#})[#Void#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: [{#i: Int#}][#Double#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseNestedStruct[#SuperBase2.BaseNestedStruct.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseNestedClass[#SuperBase2.BaseNestedClass.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseNestedUnion[#SuperBase2.BaseNestedUnion.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseExtProp[#Int#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseExtFunc0()[#Void#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseExtNestedStruct[#BaseExtNestedStruct.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseExtNestedClass[#BaseExtNestedClass.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseExtNestedUnion[#BaseExtNestedUnion.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: Keyword: .metatype[#[byref] SuperBase2.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: End completions
  }

  func test2() {
    super.#^SUPER_DOT_2^#
// SUPER_DOT_2: Begin completions
// SUPER_DOT_2-NEXT: SwiftDecl: baseInstanceVar[#Int#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: baseProp[#Int#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: baseFunc0()[#Void#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: baseFunc1({#a: Int#})[#Void#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseNestedStruct[#SuperBase2.BaseNestedStruct.metatype#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseNestedClass[#SuperBase2.BaseNestedClass.metatype#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseNestedUnion[#SuperBase2.BaseNestedUnion.metatype#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: baseExtProp[#Int#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: baseExtFunc0()[#Void#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseExtNestedStruct[#BaseExtNestedStruct.metatype#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseExtNestedClass[#BaseExtNestedClass.metatype#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseExtNestedUnion[#BaseExtNestedUnion.metatype#]{{$}}
// SUPER_DOT_2-NEXT: Keyword: metatype[#[byref] SuperBase2.metatype#]{{$}}
// SUPER_DOT_2-NEXT: End completions
  }
}

class SuperKWBase {
  func test() {
    // FIXME: make sure we don't code complete 'super' keyword here.
    #^BASE_SUPER_KW^#
  }
}

class SuperKWDerived : SuperKWBase {
  func test() {
    // FIXME: make sure include 'super' keyword in code completion results here.
    #^DERIVED_SUPER_KW^#
  }
}

//===--- Tests for 'this'.

class ThisBase1 {
  var baseInstanceVar: Int

  func baseFunc0() {}
  func baseFunc1(a: Int) {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    baseInstanceVar = i
  }

  // FIXME: uncomment when we have static vars.
  // static var baseStaticVar : Int

  static func baseStaticFunc0() {}
}

extension ThisBase1 {
  var baseExtProp : Int {
  get:
    return 42
  set(val):
  }

  func baseExtInstanceFunc0() {}

  static func baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  union BaseExtNestedUnion {
    case BaseExtUnionX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

class ThisDerived1 : ThisBase1 {
  var derivedInstanceVar: Int

  func derivedFunc0() {}

  subscript(i: Double) -> Int {
  get:
    return Int(i)
  set(val):
    baseInstanceVar = Int(i)
  }

  static func derivedStaticFunc0() {}

  func test1() {
    this#^THIS_NO_DOT_1^#
// THIS_NO_DOT_1: Begin completions
// THIS_NO_DOT_1-NEXT: SwiftDecl: .derivedInstanceVar[#Int#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .derivedFunc0()[#Void#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: [{#i: Double#}][#Int#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .test1()[#Void#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .test2()[#Void#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .derivedExtProp[#Int#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .derivedExtInstanceFunc0()[#Void#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedStruct[#DerivedExtNestedStruct.metatype#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedClass[#DerivedExtNestedClass.metatype#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedUnion[#DerivedExtNestedUnion.metatype#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .baseInstanceVar[#Int#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .baseFunc0()[#Void#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .baseFunc1({#a: Int#})[#Void#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: [{#i: Int#}][#Double#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .baseExtProp[#Int#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .baseExtInstanceFunc0()[#Void#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedStruct[#BaseExtNestedStruct.metatype#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedClass[#BaseExtNestedClass.metatype#]{{$}}
// THIS_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedUnion[#BaseExtNestedUnion.metatype#]{{$}}
// THIS_NO_DOT_1-NEXT: Keyword: .metatype[#[byref(implicit)] ThisDerived1.metatype#]{{$}}
// THIS_NO_DOT_1-NEXT: End completions
  }

  func test2() {
    this.#^THIS_DOT_1^#
// THIS_DOT_1: Begin completions
// THIS_DOT_1-NEXT: SwiftDecl: derivedInstanceVar[#Int#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: derivedFunc0()[#Void#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: test1()[#Void#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: test2()[#Void#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: derivedExtProp[#Int#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: derivedExtInstanceFunc0()[#Void#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: DerivedExtNestedStruct[#DerivedExtNestedStruct.metatype#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: DerivedExtNestedClass[#DerivedExtNestedClass.metatype#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: DerivedExtNestedUnion[#DerivedExtNestedUnion.metatype#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: baseInstanceVar[#Int#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: baseFunc0()[#Void#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: baseFunc1({#a: Int#})[#Void#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: baseExtProp[#Int#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: baseExtInstanceFunc0()[#Void#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: BaseExtNestedStruct[#BaseExtNestedStruct.metatype#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: BaseExtNestedClass[#BaseExtNestedClass.metatype#]{{$}}
// THIS_DOT_1-NEXT: SwiftDecl: BaseExtNestedUnion[#BaseExtNestedUnion.metatype#]{{$}}
// THIS_DOT_1-NEXT: Keyword: metatype[#[byref(implicit)] ThisDerived1.metatype#]{{$}}
// THIS_DOT_1-NEXT: End completions
  }

  static func staticTest1() {
    this#^THIS_STATIC_NO_DOT_1^#
// THIS_STATIC_NO_DOT_1: Begin completions
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .derivedFunc0()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: [{#i: Double#}][#Int#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .derivedStaticFunc0()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .test1()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .test2()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .staticTest1()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .staticTest2()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .derivedExtInstanceFunc0()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .derivedExtStaticFunc0()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedStruct[#DerivedExtNestedStruct.metatype#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedClass[#DerivedExtNestedClass.metatype#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedUnion[#DerivedExtNestedUnion.metatype#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedTypealias[#Int.metatype#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .baseFunc0()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .baseFunc1({#a: Int#})[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: [{#i: Int#}][#Double#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .baseStaticFunc0()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .baseExtInstanceFunc0()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .baseExtStaticFunc0()[#Void#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedStruct[#BaseExtNestedStruct.metatype#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedClass[#BaseExtNestedClass.metatype#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedUnion[#BaseExtNestedUnion.metatype#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedTypealias[#Int.metatype#]{{$}}
// Yes, '.metatype.metatype' is correct because we are in a static method.
// THIS_STATIC_NO_DOT_1-NEXT: Keyword: .metatype[#[byref(implicit)] ThisDerived1.metatype.metatype#]{{$}}
// THIS_STATIC_NO_DOT_1-NEXT: End completions
  }

  static func staticTest2() {
    this.#^THIS_STATIC_DOT_1^#
// THIS_STATIC_DOT_1: Begin completions
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: derivedFunc0()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: derivedStaticFunc0()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: test1()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: test2()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: staticTest1()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: staticTest2()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: derivedExtInstanceFunc0()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: derivedExtStaticFunc0()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: DerivedExtNestedStruct[#DerivedExtNestedStruct.metatype#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: DerivedExtNestedClass[#DerivedExtNestedClass.metatype#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: DerivedExtNestedUnion[#DerivedExtNestedUnion.metatype#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: DerivedExtNestedTypealias[#Int.metatype#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: baseFunc0()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: baseFunc1({#a: Int#})[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: baseStaticFunc0()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: baseExtInstanceFunc0()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: baseExtStaticFunc0()[#Void#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: BaseExtNestedStruct[#BaseExtNestedStruct.metatype#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: BaseExtNestedClass[#BaseExtNestedClass.metatype#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: BaseExtNestedUnion[#BaseExtNestedUnion.metatype#]{{$}}
// THIS_STATIC_DOT_1-NEXT: SwiftDecl: BaseExtNestedTypealias[#Int.metatype#]{{$}}
// Yes, '.metatype.metatype' is correct because we are in a static method.
// THIS_STATIC_DOT_1-NEXT: Keyword: metatype[#[byref(implicit)] ThisDerived1.metatype.metatype#]{{$}}
// THIS_STATIC_DOT_1-NEXT: End completions
  }
}

extension ThisDerived1 {
  var derivedExtProp : Int {
  get:
    return 42
  set(val):
  }

  func derivedExtInstanceFunc0() {}

  static func derivedExtStaticFunc0() {}

  struct DerivedExtNestedStruct {}
  class DerivedExtNestedClass {}
  union DerivedExtNestedUnion {
    case DerivedExtUnionX(Int)
  }

  typealias DerivedExtNestedTypealias = Int
}

//===--- Check that we can resolve function parameters.

func testResolveFuncParamA(fs: FooStruct) {
  fs.#^RESOLVE_FUNC_PARAM_1^#
// RESOLVE_FUNC_PARAM_1: Begin completions
// RESOLVE_FUNC_PARAM_1-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// RESOLVE_FUNC_PARAM_1-DAG: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// RESOLVE_FUNC_PARAM_1: End completions
}

func testResolveFuncParamB() {
  // FIXME: this crashes right now.
  // { (fs: FooStruct) in fs.#^RESOLVE_FUNC_PARAM_2^# }
}

class TestResolveFuncParamC {
  func testResolveFuncParamD(fs: FooStruct) {
    fs.#^RESOLVE_FUNC_PARAM_3^#
// RESOLVE_FUNC_PARAM_3: Begin completions
// RESOLVE_FUNC_PARAM_3-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// RESOLVE_FUNC_PARAM_3-DAG: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// RESOLVE_FUNC_PARAM_3: End completions
  }
}

func testResolveFuncParamE<Foo : FooProtocol>(foo: Foo) {
  foo.#^RESOLVE_FUNC_PARAM_4^#
// RESOLVE_FUNC_PARAM_4: Begin completions
// RESOLVE_FUNC_PARAM_4-NEXT: SwiftDecl: fooInstanceVar[#Int#]{{$}}
// RESOLVE_FUNC_PARAM_4-NEXT: SwiftDecl: fooInstanceFunc0()[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_4-NEXT: SwiftDecl: fooInstanceFunc1({#a: Int#})[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Keyword: metatype[#[byref(implicit)] Foo.metatype#]{{$}}
// RESOLVE_FUNC_PARAM_4-NEXT: End completions
}

func testResolveFuncParamF<FooBar : protocol<FooProtocol, BarProtocol>>(fooBar: FooBar) {
  fooBar.#^RESOLVE_FUNC_PARAM_5^#
// RESOLVE_FUNC_PARAM_5: Begin completions
// RESOLVE_FUNC_PARAM_5-NEXT: SwiftDecl: barInstanceVar[#Int#]{{$}}
// RESOLVE_FUNC_PARAM_5-NEXT: SwiftDecl: barInstanceFunc0()[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_5-NEXT: SwiftDecl: barInstanceFunc1({#a: Int#})[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_5-NEXT: SwiftDecl: fooInstanceVar[#Int#]{{$}}
// RESOLVE_FUNC_PARAM_5-NEXT: SwiftDecl: fooInstanceFunc0()[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_5-NEXT: SwiftDecl: fooInstanceFunc1({#a: Int#})[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_5-NEXT: Keyword: metatype[#[byref(implicit)] FooBar.metatype#]{{$}}
// RESOLVE_FUNC_PARAM_5-NEXT: End completions
}

func testResolveFuncParamF<FooExBarEx : protocol<FooExProtocol, BarExProtocol>>(a: FooExBarEx) {
  a.#^RESOLVE_FUNC_PARAM_6^#
// RESOLVE_FUNC_PARAM_6: Begin completions
// RESOLVE_FUNC_PARAM_6-NEXT: SwiftDecl: barInstanceVar[#Int#]{{$}}
// RESOLVE_FUNC_PARAM_6-NEXT: SwiftDecl: barInstanceFunc0()[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_6-NEXT: SwiftDecl: barInstanceFunc1({#a: Int#})[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_6-NEXT: SwiftDecl: barExInstanceFunc0()[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_6-NEXT: SwiftDecl: fooInstanceVar[#Int#]{{$}}
// RESOLVE_FUNC_PARAM_6-NEXT: SwiftDecl: fooInstanceFunc0()[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_6-NEXT: SwiftDecl: fooInstanceFunc1({#a: Int#})[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_6-NEXT: SwiftDecl: fooExInstanceFunc0()[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_6-NEXT: Keyword: metatype[#[byref(implicit)] FooExBarEx.metatype#]{{$}}
// RESOLVE_FUNC_PARAM_6-NEXT: End completions
}

func testResolveFuncParamG<Foo : FooProtocol where Foo : FooClass>(foo: Foo) {
  foo.#^RESOLVE_FUNC_PARAM_7^#
// RESOLVE_FUNC_PARAM_7: Begin completions
// RESOLVE_FUNC_PARAM_7-NEXT: SwiftDecl: fooInstanceVar[#Int#]{{$}}
// RESOLVE_FUNC_PARAM_7-NEXT: SwiftDecl: fooInstanceFunc0()[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_7-NEXT: SwiftDecl: fooInstanceFunc1({#a: Int#})[#Double#]{{$}}
// RESOLVE_FUNC_PARAM_7-NEXT: SwiftDecl: fooClassInstanceVar[#Int#]{{$}}
// RESOLVE_FUNC_PARAM_7-NEXT: SwiftDecl: fooClassInstanceFunc0()[#Void#]{{$}}
// RESOLVE_FUNC_PARAM_7-NEXT: Keyword: metatype[#[byref(implicit)] Foo.metatype#]{{$}}
// RESOLVE_FUNC_PARAM_7-NEXT: End completions
}

//===--- Check that we can handle ParenPattern in function arguments.

struct FuncParenPattern {
  constructor(_: Int) {}

  func instanceFunc(_: Int) {}

  subscript(_: Int) -> Int {
  get:
    return 0
  }
}

func testFuncParenPattern1(fpp: FuncParenPattern) {
  fpp#^FUNC_PAREN_PATTERN_1^#
// FUNC_PAREN_PATTERN_1: Begin completions
// FUNC_PAREN_PATTERN_1-NEXT: SwiftDecl: .instanceFunc({#Int#})[#Void#]{{$}}
// FUNC_PAREN_PATTERN_1-NEXT: SwiftDecl: [{#Int#}][#Int#]{{$}}
// FUNC_PAREN_PATTERN_1-NEXT: Keyword: .metatype[#[byref(implicit)] FuncParenPattern.metatype#]{{$}}
// FUNC_PAREN_PATTERN_1: End completions
}

func testFuncParenPattern2(fpp: FuncParenPattern) {
  FuncParenPattern#^FUNC_PAREN_PATTERN_2^#
// FUNC_PAREN_PATTERN_2: Begin completions
// FUNC_PAREN_PATTERN_2-NEXT: SwiftDecl: ({#Int#})[#FuncParenPattern#]{{$}}
// FUNC_PAREN_PATTERN_2-NEXT: SwiftDecl: .instanceFunc({#this: [byref] FuncParenPattern#})[#(Int) -> Void#]{{$}}
// FUNC_PAREN_PATTERN_2-NEXT: Keyword: .metatype[#FuncParenPattern.metatype.metatype#]{{$}}
// FUNC_PAREN_PATTERN_2: End completions
}

func testFuncParenPattern3(fpp: FuncParenPattern) {
  fpp.instanceFunc#^FUNC_PAREN_PATTERN_3^#
// FUNC_PAREN_PATTERN_3: Begin completions
// FUNC_PAREN_PATTERN_3-NEXT: Pattern: ({#Int#})[#Void#]{{$}}
// FUNC_PAREN_PATTERN_3-NEXT: Keyword: .metatype[#Int64 -> ().metatype#]{{$}}
// FUNC_PAREN_PATTERN_3: End completions
}

//===--- Check that we can code complete after function calls

struct SomeBuilder {
  constructor(a: Int) {}
  func doFoo() -> SomeBuilder { return this }
  func doBar() -> SomeBuilder { return this }
  func doBaz(z: Double) -> SomeBuilder { return this }
}

func testChainedCalls1() {
  SomeBuilder(42)#^CHAINED_CALLS_1^#
// CHAINED_CALLS_1: Begin completions
// CHAINED_CALLS_1-DAG: SwiftDecl: .doFoo()[#SomeBuilder#]{{$}}
// CHAINED_CALLS_1-DAG: SwiftDecl: .doBar()[#SomeBuilder#]{{$}}
// CHAINED_CALLS_1-DAG: SwiftDecl: .doBaz({#z: Double#})[#SomeBuilder#]{{$}}
// CHAINED_CALLS_1: End completions
}

func testChainedCalls2() {
  SomeBuilder(42).doFoo()#^CHAINED_CALLS_2^#
// CHAINED_CALLS_2: Begin completions
// CHAINED_CALLS_2-DAG: SwiftDecl: .doFoo()[#SomeBuilder#]{{$}}
// CHAINED_CALLS_2-DAG: SwiftDecl: .doBar()[#SomeBuilder#]{{$}}
// CHAINED_CALLS_2-DAG: SwiftDecl: .doBaz({#z: Double#})[#SomeBuilder#]{{$}}
// CHAINED_CALLS_2: End completions
}

func testChainedCalls3() {
  // doBaz() takes a Double.  Check that we can recover.
  SomeBuilder(42).doFoo().doBaz(SomeBuilder(24))#^CHAINED_CALLS_3^#
// CHAINED_CALLS_3: Begin completions
// CHAINED_CALLS_3-DAG: SwiftDecl: .doFoo()[#SomeBuilder#]{{$}}
// CHAINED_CALLS_3-DAG: SwiftDecl: .doBar()[#SomeBuilder#]{{$}}
// CHAINED_CALLS_3-DAG: SwiftDecl: .doBaz({#z: Double#})[#SomeBuilder#]{{$}}
// CHAINED_CALLS_3: End completions
}

//===--- Check that we can code complete expressions that have generic parameters

func testResolveGenericParams1() {
  FooGenericStruct<FooStruct>()#^RESOLVE_GENERIC_PARAMS_1^#
// RESOLVE_GENERIC_PARAMS_1: Begin completions
// RESOLVE_GENERIC_PARAMS_1-NEXT: SwiftDecl: .fooInstanceVarT[#T#]{{$}}
// RESOLVE_GENERIC_PARAMS_1-NEXT: SwiftDecl: .fooVoidInstanceFunc1({#a: T#})[#Void#]{{$}}
// RESOLVE_GENERIC_PARAMS_1-NEXT: SwiftDecl: .fooTInstanceFunc1({#a: T#})[#T#]{{$}}
// RESOLVE_GENERIC_PARAMS_1-NEXT: Keyword: .metatype[#FooGenericStruct<FooStruct>.metatype#]{{$}}
// RESOLVE_GENERIC_PARAMS_1-NEXT: End completions
}

func testResolveGenericParams2<Foo : FooProtocol>(foo: Foo) {
  FooGenericStruct<Foo>()#^RESOLVE_GENERIC_PARAMS_2^#
// RESOLVE_GENERIC_PARAMS_2: Begin completions
// RESOLVE_GENERIC_PARAMS_2-NEXT: SwiftDecl: .fooInstanceVarT[#T#]{{$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: SwiftDecl: .fooVoidInstanceFunc1({#a: T#})[#Void#]{{$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: SwiftDecl: .fooTInstanceFunc1({#a: T#})[#T#]{{$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: Keyword: .metatype[#FooGenericStruct<Foo>.metatype#]{{$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: End completions
}

struct TestResolveGenericParams3_4<T> {
  func testResolveGenericParams3() {
    FooGenericStruct<FooStruct>()#^RESOLVE_GENERIC_PARAMS_3^#
// RESOLVE_GENERIC_PARAMS_3: Begin completions
// RESOLVE_GENERIC_PARAMS_3-NEXT: SwiftDecl: .fooInstanceVarT[#T#]{{$}}
// RESOLVE_GENERIC_PARAMS_3-NEXT: SwiftDecl: .fooVoidInstanceFunc1({#a: T#})[#Void#]{{$}}
// RESOLVE_GENERIC_PARAMS_3-NEXT: SwiftDecl: .fooTInstanceFunc1({#a: T#})[#T#]{{$}}
// RESOLVE_GENERIC_PARAMS_3-NEXT: Keyword: .metatype[#FooGenericStruct<FooStruct>.metatype#]{{$}}
// RESOLVE_GENERIC_PARAMS_3-NEXT: End completions
  }

  func testResolveGenericParams4() {
    FooGenericStruct<T>()#^RESOLVE_GENERIC_PARAMS_4^#
// RESOLVE_GENERIC_PARAMS_4: Begin completions
// RESOLVE_GENERIC_PARAMS_4-NEXT: SwiftDecl: .fooInstanceVarT[#T#]{{$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: SwiftDecl: .fooVoidInstanceFunc1({#a: T#})[#Void#]{{$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: SwiftDecl: .fooTInstanceFunc1({#a: T#})[#T#]{{$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: Keyword: .metatype[#FooGenericStruct<T>.metatype#]{{$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: End completions
  }

  func testResolveGenericParams5<U>() {
    FooGenericStruct<U>()#^RESOLVE_GENERIC_PARAMS_5^#
// RESOLVE_GENERIC_PARAMS_5: Begin completions
// RESOLVE_GENERIC_PARAMS_5-NEXT: SwiftDecl: .fooInstanceVarT[#T#]{{$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: SwiftDecl: .fooVoidInstanceFunc1({#a: T#})[#Void#]{{$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: SwiftDecl: .fooTInstanceFunc1({#a: T#})[#T#]{{$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: Keyword: .metatype[#FooGenericStruct<U>.metatype#]{{$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: End completions
  }
}

func testResolveGenericParamsError1() {
  // There is no type 'Foo'.  Check that we don't crash.
  // FIXME: we could also display correct completion results here, because
  // swift does not have specialization, and the set of completion results does
  // not depend on the generic type argument.
  FooGenericStruct<NotDefinedType>()#^RESOLVE_GENERIC_PARAMS_ERROR_1^#
// RESOLVE_GENERIC_PARAMS_ERROR_1: found code completion token
// RESOLVE_GENERIC_PARAMS_ERROR_1-NOT: Begin completions
}

//===--- Check that we can look up into modules

func testResolveModules1() {
  swift#^RESOLVE_MODULES_1^#
// RESOLVE_MODULES_1: Begin completions
// RESOLVE_MODULES_1-DAG: SwiftDecl: .true[#Bool#]{{$}}
// RESOLVE_MODULES_1-DAG: SwiftDecl: .false[#Bool#]{{$}}
// RESOLVE_MODULES_1-DAG: SwiftDecl: .Int8[#Int8.metatype#]{{$}}
// RESOLVE_MODULES_1-DAG: SwiftDecl: .Int16[#Int16.metatype#]{{$}}
// RESOLVE_MODULES_1-DAG: SwiftDecl: .Int32[#Int32.metatype#]{{$}}
// RESOLVE_MODULES_1-DAG: SwiftDecl: .Int64[#Int64.metatype#]{{$}}
// RESOLVE_MODULES_1-DAG: SwiftDecl: .Int128[#Int128.metatype#]{{$}}
// RESOLVE_MODULES_1-DAG: SwiftDecl: .Bool[#Bool.metatype#]{{$}}
// RESOLVE_MODULES_1-DAG: SwiftDecl: .Float[#Float32.metatype#]{{$}}
// RESOLVE_MODULES_1: End completions
}

