enum CustomError: Error {
  case invalid
  case insecure
}

typealias SomeCallback = (String) -> Void

func simple(completion: (String) -> Void) { }
func simpleWithArg(a: Int, completion: (String) -> Void) { }
func multipleResults(completion: (String, Int) -> Void) { }
func nonOptionalError(completion: (String, Error) -> Void) { }
func noParams(completion: () -> Void) { }
func error(completion: (String?, Error?) -> Void) { }
func errorOnly(completion: (Error?) -> Void) { }
func errorNonOptionalResult(completion: (String, Error?) -> Void) { }
func alias(completion: SomeCallback) { }
func simpleResult(completion: (Result<String, Never>) -> Void) { }
func errorResult(completion: (Result<String, Error>) -> Void) { }
func customErrorResult(completion: (Result<String, CustomError>) -> Void) { }
func optionalSingle(completion: (String?) -> Void) { }
func manyOptional(_ completion: (String?, Int?) -> Void) { }
func generic<T, R>(completion: (T, R) -> Void) { }
func genericResult<T>(completion: (T?, Error?) -> Void) where T: Numeric { }
func genericError<E>(completion: (String?, E?) -> Void) where E: Error { }
func defaultArgs(a: Int, b: Int = 10, completion: (String) -> Void) { }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=SIMPLE-WITH-VARIABLE-COMPLETION-HANDLER %s
func testSimpleWithVariableCompletionHandler(completionHandler: (String) -> Void) {
  simple(completion: completionHandler)
}
// SIMPLE-WITH-VARIABLE-COMPLETION-HANDLER: let result = await simple()
// SIMPLE-WITH-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=SIMPLE-WITH-ARG-VARIABLE-COMPLETION-HANDLER %s
func testSimpleWithArgVariableCompletionHandler(b: Int, completionHandler: (String) -> Void) {
  simpleWithArg(a: b, completion: completionHandler)
}
// SIMPLE-WITH-ARG-VARIABLE-COMPLETION-HANDLER: let result = await simpleWithArg(a: b)
// SIMPLE-WITH-ARG-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=SIMPLE-WITH-CONSTANT-ARG-VARIABLE-COMPLETION-HANDLER %s
func testSimpleWithConstantArgVariableCompletionHandler(completionHandler: (String) -> Void) {
  simpleWithArg(a: 1, completion: completionHandler)
}
// SIMPLE-WITH-CONSTANT-ARG-VARIABLE-COMPLETION-HANDLER: let result = await simpleWithArg(a: 1)
// SIMPLE-WITH-CONSTANT-ARG-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=MULTIPLE-RESULTS-VARIABLE-COMPLETION-HANDLER %s
func testMultipleResultsVariableCompletionHandler(completionHandler: (String, Int) -> Void) {
  multipleResults(completion: completionHandler)
}
// MULTIPLE-RESULTS-VARIABLE-COMPLETION-HANDLER: let result = await multipleResults()
// MULTIPLE-RESULTS-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result.0, result.1)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=NON-OPTIONAL-ERROR-VARIABLE-COMPLETION-HANDLER %s
func testNonOptionalErrorVariableCompletionHandler(completionHandler: (String, Error) -> Void) {
  nonOptionalError(completion: completionHandler)
}
// NON-OPTIONAL-ERROR-VARIABLE-COMPLETION-HANDLER: let result = await nonOptionalError()
// NON-OPTIONAL-ERROR-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result.0, result.1)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=NO-PARAMS-VARIABLE-COMPLETION-HANDLER %s
func testNoParamsVariableCompletionHandler(completionHandler: () -> Void) {
  noParams(completion: completionHandler)
}
// NO-PARAMS-VARIABLE-COMPLETION-HANDLER: await noParams()
// NO-PARAMS-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler()

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=ERROR-VARIABLE-COMPLETION-HANDLER %s
func testErrorWithVariableCompletionHandler(completionHandler: (String?, Error?) -> Void) {
  error(completion: completionHandler)
}
// ERROR-VARIABLE-COMPLETION-HANDLER: do {
// ERROR-VARIABLE-COMPLETION-HANDLER-NEXT:   let result = try await error()
// ERROR-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(result, nil)
// ERROR-VARIABLE-COMPLETION-HANDLER-NEXT: } catch {
// ERROR-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(nil, error)
// ERROR-VARIABLE-COMPLETION-HANDLER-NEXT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=ERROR-ONLY-VARIABLE-COMPLETION-HANDLER %s
func testErrorOnlyWithVariableCompletionHandler(completionHandler: (Error?) -> Void) {
  errorOnly(completion: completionHandler)

}
// ERROR-ONLY-VARIABLE-COMPLETION-HANDLER: do {
// ERROR-ONLY-VARIABLE-COMPLETION-HANDLER-NEXT:   try await errorOnly()
// ERROR-ONLY-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(nil)
// ERROR-ONLY-VARIABLE-COMPLETION-HANDLER-NEXT: } catch {
// ERROR-ONLY-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(error)
// ERROR-ONLY-VARIABLE-COMPLETION-HANDLER-NEXT: }

// FIXME: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3
func testErrorNonOptionalResultWithVariableCompletionHandler(completionHandler: (String, Error?) -> Void) {
  errorNonOptionalResult(completion: completionHandler)
}

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=ALIAS-VARIABLE-COMPLETION-HANDLER %s
func testAliasWithVariableCompletionHandler(completionHandler: SomeCallback) {
  alias(completion: completionHandler)
}
// ALIAS-VARIABLE-COMPLETION-HANDLER: let result = await alias()
// ALIAS-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=SIMPLE-RESULT-VARIABLE-COMPLETION-HANDLER %s
func testSimpleResultVariableCompletionHandler(completionHandler: (Result<String, Never>) -> Void) {
  simpleResult(completion: completionHandler)
}
// SIMPLE-RESULT-VARIABLE-COMPLETION-HANDLER: let result = await simpleResult()
// SIMPLE-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(.success(result))

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=ERROR-RESULT-VARIABLE-COMPLETION-HANDLER %s
func testErrorResultVariableCompletionHandler(completionHandler: (Result<String, Error>) -> Void) {
  errorResult(completion: completionHandler)
}
// ERROR-RESULT-VARIABLE-COMPLETION-HANDLER: do {
// ERROR-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT:   let result = try await errorResult()
// ERROR-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(.success(result))
// ERROR-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT: } catch {
// ERROR-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(.failure(error))
// ERROR-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=CUSTOM-ERROR-RESULT-VARIABLE-COMPLETION-HANDLER %s
func testErrorResultVariableCompletionHandler(completionHandler: (Result<String, CustomError>) -> Void) {
  customErrorResult(completion: completionHandler)
}
// CUSTOM-ERROR-RESULT-VARIABLE-COMPLETION-HANDLER: do {
// CUSTOM-ERROR-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT:   let result = try await customErrorResult()
// CUSTOM-ERROR-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(.success(result))
// CUSTOM-ERROR-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT: } catch {
// CUSTOM-ERROR-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(.failure(error as! CustomError))
// CUSTOM-ERROR-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=OPTIONAL-SINGLE-VARIABLE-COMPLETION-HANDLER %s
func testOptionalSingleVariableCompletionHandler(completionHandler: (String?) -> Void) {
  optionalSingle(completion: completionHandler)
}
// OPTIONAL-SINGLE-VARIABLE-COMPLETION-HANDLER: let result = await optionalSingle()
// OPTIONAL-SINGLE-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=MANY-OPTIONAL-VARIABLE-COMPLETION-HANDLER %s
func testManyOptionalVariableCompletionHandler(completionHandler: (String?, Int?) -> Void) {
  manyOptional(completionHandler)
}
// MANY-OPTIONAL-VARIABLE-COMPLETION-HANDLER: let result = await manyOptional()
// MANY-OPTIONAL-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result.0, result.1)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=GENERIC-VARIABLE-COMPLETION-HANDLER %s
func testGenericVariableCompletionHandler<T, R>(completionHandler: (T, R) -> Void) {
  generic(completion: completionHandler)
}
// GENERIC-VARIABLE-COMPLETION-HANDLER: let result: (T, R) = await generic()
// GENERIC-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result.0, result.1)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=SPECIALIZE-GENERIC-VARIABLE-COMPLETION-HANDLER %s
func testSpecializeGenericsVariableCompletionHandler(completionHandler: (String, Int) -> Void) {
  generic(completion: completionHandler)
}
// SPECIALIZE-GENERIC-VARIABLE-COMPLETION-HANDLER: let result: (String, Int) = await generic()
// SPECIALIZE-GENERIC-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result.0, result.1)

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=GENERIC-RESULT-VARIABLE-COMPLETION-HANDLER %s
func testGenericResultVariableCompletionHandler<T>(completionHandler: (T?, Error?) -> Void) where T: Numeric {
  genericResult(completion: completionHandler)
}
// GENERIC-RESULT-VARIABLE-COMPLETION-HANDLER: do {
// GENERIC-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT:   let result: T = try await genericResult()
// GENERIC-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(result, nil)
// GENERIC-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT: } catch {
// GENERIC-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(nil, error)
// GENERIC-RESULT-VARIABLE-COMPLETION-HANDLER-NEXT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=GENERIC-ERROR-VARIABLE-COMPLETION-HANDLER %s
func testGenericErrorVariableCompletionHandler<MyGenericError>(completionHandler: (String?, MyGenericError?) -> Void) where MyGenericError: Error {
  genericError(completion: completionHandler)
}
// GENERIC-ERROR-VARIABLE-COMPLETION-HANDLER: do {
// GENERIC-ERROR-VARIABLE-COMPLETION-HANDLER-NEXT:   let result: String = try await genericError()
// GENERIC-ERROR-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(result, nil)
// GENERIC-ERROR-VARIABLE-COMPLETION-HANDLER-NEXT: } catch {
// GENERIC-ERROR-VARIABLE-COMPLETION-HANDLER-NEXT:   completionHandler(nil, error as! MyGenericError)
// GENERIC-ERROR-VARIABLE-COMPLETION-HANDLER-NEXT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=DEFAULT-ARGS-VARIABLE-COMPLETION-HANDLER %s
func testDefaultArgsVariableCompletionHandler(completionHandler: (String) -> Void) {
  defaultArgs(a: 5, completion: completionHandler)
}
// DEFAULT-ARGS-VARIABLE-COMPLETION-HANDLER: let result = await defaultArgs(a: 5)
// DEFAULT-ARGS-VARIABLE-COMPLETION-HANDLER-NEXT: completionHandler(result)

func myPrint(_ message: String) {
  print(message)
}

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):3 | %FileCheck -check-prefix=GLOBAL-FUNC-AS-COMPLETION-HANDLER %s
func testGlobalFuncAsCompletionHandler() {
  simple(completion: myPrint)
}
// GLOBAL-FUNC-AS-COMPLETION-HANDLER: let result = await simple()
// GLOBAL-FUNC-AS-COMPLETION-HANDLER-NEXT: myPrint(result)

class Foo {
  var foo: Foo

  init(foo: Foo) {
    self.foo = foo
  }

  func myFooPrint(_ message: String) {
    print("FOO: \(message)")
  }

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):5 | %FileCheck -check-prefix=MEMBER-FUNC-AS-COMPLETION-HANDLER %s
  func testMethodAsCompletionHandler() {
    simple(completion: myFooPrint)
  }
  // MEMBER-FUNC-AS-COMPLETION-HANDLER: let result = await simple()
  // MEMBER-FUNC-AS-COMPLETION-HANDLER-NEXT: myFooPrint(result)

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):5 | %FileCheck -check-prefix=MEMBER-FUNC-ON-OTHER-OBJECT-AS-COMPLETION-HANDLER %s
  func testMethodOnOtherObjectAsCompletionHandler(foo: Foo) {
    simple(completion: foo.myFooPrint)
  }
  // MEMBER-FUNC-ON-OTHER-OBJECT-AS-COMPLETION-HANDLER: let result = await simple()
  // MEMBER-FUNC-ON-OTHER-OBJECT-AS-COMPLETION-HANDLER-NEXT: foo.myFooPrint(result)

  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+2):5 | %FileCheck -check-prefix=MEMBER-FUNC-ON-NESTED-OTHER-OBJECT-AS-COMPLETION-HANDLER %s
  func testMethodOnNestedOtherObjectAsCompletionHandler(foo: Foo) {
    simple(completion: foo.foo.myFooPrint)
  }
  // MEMBER-FUNC-ON-NESTED-OTHER-OBJECT-AS-COMPLETION-HANDLER: let result = await simple()
  // MEMBER-FUNC-ON-NESTED-OTHER-OBJECT-AS-COMPLETION-HANDLER-NEXT: foo.foo.myFooPrint(result)

}
