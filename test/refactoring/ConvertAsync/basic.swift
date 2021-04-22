enum CustomError: Error {
  case invalid
  case insecure
}

typealias SomeCallback = (String) -> Void
typealias SomeResultCallback = (Result<String, CustomError>) -> Void
typealias NestedAliasCallback = SomeCallback

// 1. Check various functions for having/not having async alternatives

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+4):1 | %FileCheck -check-prefix=ASYNC-SIMPLE %s
// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+3):6 | %FileCheck -check-prefix=ASYNC-SIMPLE %s
// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+2):12 | %FileCheck -check-prefix=ASYNC-SIMPLE %s
// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):13 | %FileCheck -check-prefix=ASYNC-SIMPLE %s
func simple(completion: (String) -> Void) { }
// ASYNC-SIMPLE: basic.swift [[# @LINE-1]]:1 -> [[# @LINE-1]]:1
// ASYNC-SIMPLE-NEXT: @available(*, deprecated, message: "Prefer async alternative instead")
// ASYNC-SIMPLE-EMPTY:
// ASYNC-SIMPLE-NEXT: basic.swift [[# @LINE-4]]:46 -> [[# @LINE-4]]:46
// ASYNC-SIMPLE-EMPTY:
// ASYNC-SIMPLE-EMPTY:
// ASYNC-SIMPLE-EMPTY:
// ASYNC-SIMPLE-NEXT: basic.swift [[# @LINE-8]]:46 -> [[# @LINE-8]]:46
// ASYNC-SIMPLE-NEXT: func simple() async -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-SIMPLENOLABEL %s
func simpleWithoutLabel(_ completion: (String) -> Void) { }
// ASYNC-SIMPLENOLABEL: func simpleWithoutLabel() async -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-SIMPLEWITHARG %s
func simpleWithArg(a: Int, completion: (String) -> Void) { }
// ASYNC-SIMPLEWITHARG: func simpleWithArg(a: Int) async -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-MULTIPLERESULTS %s
func multipleResults(completion: (String, Int) -> Void) { }
// ASYNC-MULTIPLERESULTS: func multipleResults() async -> (String, Int) { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-NONOPTIONALERROR %s
func nonOptionalError(completion: (String, Error) -> Void) { }
// ASYNC-NONOPTIONALERROR: func nonOptionalError() async -> (String, Error) { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-NOPARAMS %s
func noParams(completion: () -> Void) { }
// ASYNC-NOPARAMS: func noParams() async { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-ERROR %s
func error(completion: (String?, Error?) -> Void) { }
// ASYNC-ERROR: func error() async throws -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-ERRORONLY %s
func errorOnly(completion: (Error?) -> Void) { }
// ASYNC-ERRORONLY: func errorOnly() async throws { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-ERRORNONOPTIONALRESULT %s
func errorNonOptionalResult(completion: (String, Error?) -> Void) { }
// ASYNC-ERRORNONOPTIONALRESULT: func errorNonOptionalResult() async throws -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-CUSTOMERROR %s
func customError(completion: (String?, CustomError?) -> Void) { }
// ASYNC-CUSTOMERROR: func customError() async throws -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-ALIAS %s
func alias(completion: SomeCallback) { }
// ASYNC-ALIAS: func alias() async -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-NESTEDALIAS %s
func nestedAlias(completion: NestedAliasCallback) { }
// ASYNC-NESTEDALIAS: func nestedAlias() async -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-SIMPLERESULT %s
func simpleResult(completion: (Result<String, Never>) -> Void) { }
// ASYNC-SIMPLERESULT: func simpleResult() async -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-ERRORRESULT %s
func errorResult(completion: (Result<String, Error>) -> Void) { }
// ASYNC-ERRORRESULT: func errorResult() async throws -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-CUSTOMERRORRESULT %s
func customErrorResult(completion: (Result<String, CustomError>) -> Void) { }
// ASYNC-CUSTOMERRORRESULT: func customErrorResult() async throws -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-ALIASRESULT %s
func aliasedResult(completion: SomeResultCallback) { }
// ASYNC-ALIASRESULT: func aliasedResult() async throws -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=MANY %s
func many(_ completion: (String, Int) -> Void) { }
// MANY: func many() async -> (String, Int) { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=OPTIONAL-SINGLE %s
func optionalSingle(completion: (String?) -> Void) { }
// OPTIONAL-SINGLE: func optionalSingle() async -> String? { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=MANY-OPTIONAL %s
func manyOptional(_ completion: (String?, Int?) -> Void) { }
// MANY-OPTIONAL: func manyOptional() async -> (String?, Int?) { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=GENERIC %s
func generic<T, R>(completion: (T, R) -> Void) { }
// GENERIC: func generic<T, R>() async -> (T, R) { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=GENERIC-RESULT %s
func genericResult<T>(completion: (T?, Error?) -> Void) where T: Numeric { }
// GENERIC-RESULT: func genericResult<T>() async throws -> T where T: Numeric { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=GENERIC-ERROR %s
func genericError<E>(completion: (String?, E?) -> Void) where E: Error { }
// GENERIC-ERROR: func genericError<E>() async throws -> String where E: Error { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=OTHER-NAME %s
func otherName(execute: (String) -> Void) { }
// OTHER-NAME: func otherName() async -> String { }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=DEFAULT_ARGS %s
func defaultArgs(a: Int, b: Int = 10, completion: (String) -> Void) { }
// DEFAULT_ARGS: func defaultArgs(a: Int, b: Int = 10) async -> String { }

struct MyStruct {
  var someVar: (Int) -> Void {
    get {
      return {_ in }
    }
    // RUN: not %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):5
    set (completion) {
    }
  }

  init() { }

  // RUN: not %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):3
  init(completion: (String) -> Void) { }

  func retSelf() -> MyStruct { return self }

  // RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):10 | %FileCheck -check-prefix=MODIFIERS %s
  public func publicMember(completion: (String) -> Void) { }
  // MODIFIERS: public func publicMember() async -> String { }

  // RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=STATIC %s
  static func staticMember(completion: (String) -> Void) { }
  // STATIC: static func staticMember() async -> String { }

  // RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+2):11 | %FileCheck -check-prefix=DEPRECATED %s
  @available(*, deprecated, message: "Deprecated")
  private func deprecated(completion: (String) -> Void) { }
  // DEPRECATED: @available(*, deprecated, message: "Deprecated")
  // DEPRECATED-NEXT: private func deprecated() async -> String { }
}
func retStruct() -> MyStruct { return MyStruct() }

protocol MyProtocol {
  // RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=PROTO-MEMBER %s
  // RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=PROTO-MEMBER-TO-ASYNC %s
  func protoMember(completion: (String) -> Void)
  // PROTO-MEMBER: func protoMember() async -> String{{$}}

  // FIXME: The current async refactoring only refactors the client side and thus only adds the 'async' keyword.
  // We should be refactoring the entire method signature here and removing the completion parameter.
  // This test currently checks that we are not crashing.
  // PROTO-MEMBER-TO-ASYNC: func protoMember(completion: (String) -> Void) async
}

// RUN: not %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
func nonCompletion(a: Int) { }

// RUN: not %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
func multipleResults(completion: (Result<String, Error>, Result<String, Error>) -> Void) { }

// RUN: not %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
func completionNotLast(completion: (String) -> Void, a: Int) { }

// RUN: not %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
func nonVoid(completion: (String) -> Void) -> Int { return 0 }

// RUN: not %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
func completionNonVoid(completion: (String) -> Int) -> Void { }

// RUN: not %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
func alreadyThrows(completion: (String) -> Void) throws { }

// RUN: not %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
func noParamAutoclosure(completion: @autoclosure () -> Void) { }

// 2. Check that the various ways to call a function (and the positions the
//    refactoring is called from) are handled correctly

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefixes=CONVERT-FUNC,CALL,CALL-NOLABEL,CALL-WRAPPED,TRAILING,TRAILING-PARENS,TRAILING-WRAPPED,CALL-ARG,MANY-CALL,MEMBER-CALL,MEMBER-CALL2,MEMBER-PARENS,EMPTY-CAPTURE,CAPTURE,DEFAULT-ARGS-MISSING,DEFAULT-ARGS-CALL %s
func testCalls() {
// CONVERT-FUNC: {{^}}func testCalls() async {
  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+4):3 | %FileCheck -check-prefix=CALL %s
  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+3):10
  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):24
  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):28
  simple(completion: { str in
    // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):5
    print("with label")
  })
  // CALL: let str = await simple(){{$}}
  // CALL-NEXT: {{^}}print("with label")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=CALL-NOLABEL %s
  simpleWithoutLabel({ str in
    print("without label")
  })
  // CALL-NOLABEL: let str = await simpleWithoutLabel(){{$}}
  // CALL-NOLABEL-NEXT: {{^}}print("without label")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=CALL-WRAPPED %s
  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):5 | %FileCheck -check-prefix=CALL-WRAPPED %s
  ((simple))(completion: { str in
    print("wrapped call")
  })
  // CALL-WRAPPED: let str = await ((simple))(){{$}}
  // CALL-WRAPPED-NEXT: {{^}}print("wrapped call")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=TRAILING %s
  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):12
  simple { str in
    print("trailing")
  }
  // TRAILING: let str = await simple(){{$}}
  // TRAILING-NEXT: {{^}}print("trailing")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=TRAILING-PARENS %s
  simple() { str in
    print("trailing with parens")
  }
  // TRAILING-PARENS: let str = await simple(){{$}}
  // TRAILING-PARENS-NEXT: {{^}}print("trailing with parens")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):5 | %FileCheck -check-prefix=TRAILING-WRAPPED %s
  ((simple)) { str in
    print("trailing with wrapped call")
  }
  // TRAILING-WRAPPED: let str = await ((simple))(){{$}}
  // TRAILING-WRAPPED-NEXT: {{^}}print("trailing with wrapped call")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+3):3 | %FileCheck -check-prefix=CALL-ARG %s
  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):17
  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):20
  simpleWithArg(a: 10) { str in
    print("with arg")
  }
  // CALL-ARG: let str = await simpleWithArg(a: 10){{$}}
  // CALL-ARG-NEXT: {{^}}print("with arg")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=MANY-CALL %s
  many { str, num in
    print("many")
  }
  // MANY-CALL: let (str, num) = await many(){{$}}
  // MANY-CALL-NEXT: {{^}}print("many")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):15 | %FileCheck -check-prefix=MEMBER-CALL %s
  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=MEMBER-CALL %s
  retStruct().publicMember { str in
    print("call on member")
  }
  // MEMBER-CALL: let str = await retStruct().publicMember(){{$}}
  // MEMBER-CALL-NEXT: {{^}}print("call on member")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):25 | %FileCheck -check-prefix=MEMBER-CALL2 %s
  retStruct().retSelf().publicMember { str in
    print("call on member 2")
  }
  // MEMBER-CALL2: let str = await retStruct().retSelf().publicMember(){{$}}
  // MEMBER-CALL2-NEXT: {{^}}print("call on member 2")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+3):3 | %FileCheck -check-prefix=MEMBER-PARENS %s
  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):5 | %FileCheck -check-prefix=MEMBER-PARENS %s
  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):15 | %FileCheck -check-prefix=MEMBER-PARENS %s
  (((retStruct().retSelf()).publicMember)) { str in
    print("call on member parens")
  }
  // MEMBER-PARENS: let str = await (((retStruct().retSelf()).publicMember))(){{$}}
  // MEMBER-PARENS-NEXT: {{^}}print("call on member parens")

  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):13
  let _: Void = simple { str in
    print("assigned")
  }
  // CONVERT-FUNC: let _: Void = simple { str in{{$}}
  // CONVERT-FUNC-NEXT: print("assigned"){{$}}
  // CONVERT-FUNC-NEXT: }{{$}}

  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3
  noParamAutoclosure(completion: print("autoclosure"))
  // CONVERT-FUNC: noParamAutoclosure(completion: print("autoclosure")){{$}}

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=EMPTY-CAPTURE %s
  simple { [] str in
    print("closure with empty capture list")
  }
  // EMPTY-CAPTURE: let str = await simple(){{$}}
  // EMPTY-CAPTURE-NEXT: {{^}}print("closure with empty capture list")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=CAPTURE %s
  let anything = "anything"
  simple { [unowned anything] str in
    print("closure with capture list \(anything)")
  }
  // CAPTURE: let str = await simple(){{$}}
  // CAPTURE-NEXT: {{^}}print("closure with capture list \(anything)")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=OTHER-DIRECT %s
  otherName(execute: { str in
    print("otherName")
  })
  // OTHER-DIRECT: let str = await otherName(){{$}}
  // OTHER-DIRECT-NEXT: {{^}}print("otherName")
  // CONVERT-FUNC: otherName(execute: { str in{{$}}
  // CONVERT-FUNC-NEXT: print("otherName"){{$}}
  // CONVERT-FUNC-NEXT: }){{$}}

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=DEFAULT-ARGS-MISSING %s
  defaultArgs(a: 1) { str in
    print("defaultArgs missing")
  }
  // DEFAULT-ARGS-MISSING: let str = await defaultArgs(a: 1){{$}}
  // DEFAULT-ARGS-MISSING-NEXT: {{^}}print("defaultArgs missing")

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=DEFAULT-ARGS-CALL %s
  defaultArgs(a: 1, b: 2) { str in
    print("defaultArgs")
  }
  // DEFAULT-ARGS-CALL: let str = await defaultArgs(a: 1, b: 2){{$}}
  // DEFAULT-ARGS-CALL-NEXT: {{^}}print("defaultArgs")
}
// CONVERT-FUNC: {{^}}}
