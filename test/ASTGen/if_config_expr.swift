// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen -D CONDITION_1

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

postfix operator ++
postfix func ++ (_: Int) -> Int { 0 }

struct OneResult {}
struct TwoResult {}

protocol MyProto {
    func optionalMethod() -> [Int]?
}
struct MyStruct {
    var optionalMember: MyProto? { nil }
    func methodOne() -> OneResult { OneResult() }
    func methodTwo() -> TwoResult { TwoResult() }
}

func globalFunc<T>(_ arg: T) -> T { arg }

func testBasic(baseExpr: MyStruct) {
    baseExpr
#if CONDITION_1
      .methodOne() // expected-warning {{result of call to 'methodOne()' is unused}}
#else
      .methodTwo()
#endif
}

MyStruct()
#if CONDITION_1
  .methodOne() // expected-warning {{result of call to 'methodOne()' is unused}}
#else
  .methodTwo()
#endif


func testInvalidContent(baseExpr: MyStruct, otherExpr: Int) {
  baseExpr      // expected-warning {{expression of type 'MyStruct' is unused}}
#if CONDITION_1
    { print(1) } // expected-error {{closure expression is unused}}
                 // unexpected-note@-1 {{did you mean to use a 'do' statement?}}
#endif

  baseExpr      // expected-warning {{expression of type 'MyStruct' is unused}}
#if CONDITION_1
    + otherExpr // expected-error {{unexpected code '+ otherExpr' in conditional compilation block}}
#endif

  baseExpr
#if CONDITION_1
    .methodOne() // expected-warning {{result of call to 'methodOne()' is unused}}

  print("debug") // expected-error {{unexpected code 'print("debug")' in conditional compilation block}}
#endif
}

func testExprKind(baseExpr: MyStruct, idx: Int) {
  baseExpr
#if CONDITION_1
  .optionalMember?.optionalMethod()![idx]++ // expected-warning {{result of operator '++' is unused}}
#else
  .otherMethod(arg) {
    //...
  }
#endif

  baseExpr
#if CONDITION_1
  .methodOne() + 12 // expected-error {{unexpected code '+ 12' in conditional compilation block}}
                    // expected-warning@-1 {{result of call to 'methodOne()' is unused}}
#endif
}

func emptyElse(baseExpr: MyStruct) {
  baseExpr
#if CONDITION_1
    .methodOne() // expected-warning {{result of call to 'methodOne()' is unused}}
#elseif CONDITION_2
    // OK. Do nothing.
#endif

  baseExpr
#if CONDITION_1
    .methodOne() // expected-warning {{result of call to 'methodOne()' is unused}}
#elseif CONDITION_2
  return         // expected-error {{unexpected 'return' keyword in conditional compilation block}}
#endif
}

func consecutiveIfConfig(baseExpr: MyStruct) {
    baseExpr
#if CONDITION_1
  .methodOne()
#endif
#if CONDITION_2
  .methodTwo()
#endif
  .unknownMethod() // expected-error {{value of type 'OneResult' has no member 'unknownMethod'}}
}

func nestedIfConfig(baseExpr: MyStruct) {
  baseExpr
#if CONDITION_1
  #if CONDITION_2
    .methodOne()
  #endif
  #if CONDITION_1
    .methodTwo() // expected-warning {{result of call to 'methodTwo()' is unused}}
  #endif
#else
  .unknownMethod1()
  #if CONDITION_2
    .unknownMethod2()
  #endif
#endif
}

func ifconfigExprInExpr(baseExpr: MyStruct) {
  globalFunc( // expected-warning {{result of call to 'globalFunc' is unused}}
    baseExpr
#if CONDITION_1
      .methodOne()
#else
      .methodTwo()
#endif
  )
}
