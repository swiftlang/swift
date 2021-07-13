// REQUIRES: concurrency

// RUN: %empty-directory(%t)

enum CustomError : Error {
  case Bad
}

func simple(_ completion: @escaping (String) -> Void) { }
func simple2(arg: String, _ completion: @escaping (String) -> Void) { }
func simpleErr(arg: String, _ completion: @escaping (String?, Error?) -> Void) { }
func simpleRes(arg: String, _ completion: @escaping (Result<String, Error>) -> Void) { }
func run(block: () -> Bool) -> Bool { return false }

func makeOptionalError() -> Error? { return nil }
func makeOptionalString() -> String? { return nil }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NESTED %s
func nested() {
  simple {
    simple2(arg: $0) { str2 in
      print(str2)
    }
  }
}
// NESTED: func nested() async {
// NESTED-NEXT: let val0 = await simple()
// NESTED-NEXT: let str2 = await simple2(arg: val0)
// NESTED-NEXT: print(str2)
// NESTED-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+2):9 | %FileCheck -check-prefix=ATTRIBUTES %s
@available(*, deprecated, message: "Deprecated")
private func functionWithAttributes() {
  simple { str in
    print(str)
  }
}
// ATTRIBUTES: convert_function.swift [[# @LINE-6]]:1 -> [[# @LINE-1]]:2
// ATTRIBUTES-NEXT: @available(*, deprecated, message: "Deprecated")
// ATTRIBUTES-NEXT: private func functionWithAttributes() async {
// ATTRIBUTES-NEXT: let str = await simple()
// ATTRIBUTES-NEXT: print(str)
// ATTRIBUTES-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=MANY-NESTED %s
func manyNested() {
  simple { str1 in
    print("simple")
    simple2(arg: str1) { str2 in
      print("simple2")
      simpleErr(arg: str2) { str3, err in
        print("simpleErr")
        guard let str3 = str3, err == nil else {
          return
        }
        simpleRes(arg: str3) { res in
          print("simpleRes")
          if case .success(let str4) = res {
            print("\(str1) \(str2) \(str3) \(str4)")
            print("after")
          }
        }
      }
    }
  }
}
// MANY-NESTED: func manyNested() async {
// MANY-NESTED-NEXT: let str1 = await simple()
// MANY-NESTED-NEXT: print("simple")
// MANY-NESTED-NEXT: let str2 = await simple2(arg: str1)
// MANY-NESTED-NEXT: print("simple2")
// MANY-NESTED-NEXT: let str3 = try await simpleErr(arg: str2)
// MANY-NESTED-NEXT: print("simpleErr")
// MANY-NESTED-NEXT: let str4 = try await simpleRes(arg: str3)
// MANY-NESTED-NEXT: print("simpleRes")
// MANY-NESTED-NEXT: print("\(str1) \(str2) \(str3) \(str4)")
// MANY-NESTED-NEXT: print("after")
// MANY-NESTED-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+2):1 | %FileCheck -check-prefix=ASYNC-SIMPLE %s
// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-SIMPLE %s
func asyncParams(arg: String, _ completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: arg) { str, err in
    print("simpleErr")
    guard let str = str, err == nil else {
      completion(nil, err!)
      return
    }
    completion(str, nil)
    print("after")
  }
}
// ASYNC-SIMPLE: func {{[a-zA-Z_]+}}(arg: String) async throws -> String {
// ASYNC-SIMPLE-NEXT: let str = try await simpleErr(arg: arg)
// ASYNC-SIMPLE-NEXT: print("simpleErr")
// ASYNC-SIMPLE-NEXT: {{^}}return str{{$}}
// ASYNC-SIMPLE-NEXT: print("after")
// ASYNC-SIMPLE-NEXT: }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-SIMPLE %s
func asyncResErrPassed(arg: String, _ completion: @escaping (Result<String, Error>) -> Void) {
  simpleErr(arg: arg) { str, err in
    print("simpleErr")
    guard let str = str, err == nil else {
      completion(.failure(err!))
      return
    }
    completion(.success(str))
    print("after")
  }
}

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-ERR %s
func asyncResNewErr(arg: String, _ completion: @escaping (Result<String, Error>) -> Void) {
  simpleErr(arg: arg) { str, err in
    print("simpleErr")
    guard let str = str, err == nil else {
      completion(.failure(CustomError.Bad))
      return
    }
    completion(.success(str))
    print("after")
  }
}
// ASYNC-ERR: func asyncResNewErr(arg: String) async throws -> String {
// ASYNC-ERR-NEXT: do {
// ASYNC-ERR-NEXT: let str = try await simpleErr(arg: arg)
// ASYNC-ERR-NEXT: print("simpleErr")
// ASYNC-ERR-NEXT: {{^}}return str{{$}}
// ASYNC-ERR-NEXT: print("after")
// ASYNC-ERR-NEXT: } catch let err {
// ASYNC-ERR-NEXT: throw CustomError.Bad
// ASYNC-ERR-NEXT: }
// ASYNC-ERR-NEXT: }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=CALL-NON-ASYNC-IN-ASYNC %s
func callNonAsyncInAsync(_ completion: @escaping (String) -> Void) {
  simple { str in
    let success = run {
      completion(str)
      return true
    }
    if !success {
      completion("bad")
    }
  }
}
// CALL-NON-ASYNC-IN-ASYNC:      func callNonAsyncInAsync() async -> String {
// CALL-NON-ASYNC-IN-ASYNC-NEXT:   let str = await simple()
// CALL-NON-ASYNC-IN-ASYNC-NEXT:   return await withCheckedContinuation { continuation in
// CALL-NON-ASYNC-IN-ASYNC-NEXT:     let success = run {
// CALL-NON-ASYNC-IN-ASYNC-NEXT:       continuation.resume(returning: str)
// CALL-NON-ASYNC-IN-ASYNC-NEXT:       {{^}} return true{{$}}
// CALL-NON-ASYNC-IN-ASYNC-NEXT:     }
// CALL-NON-ASYNC-IN-ASYNC-NEXT:     if !success {
// CALL-NON-ASYNC-IN-ASYNC-NEXT:       continuation.resume(returning: "bad")
// CALL-NON-ASYNC-IN-ASYNC-NEXT:     }
// CALL-NON-ASYNC-IN-ASYNC-NEXT:   }
// CALL-NON-ASYNC-IN-ASYNC-NEXT: }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=CALL-NON-ASYNC-IN-ASYNC-COMMENT %s
func callNonAsyncInAsyncComment(_ completion: @escaping (String) -> Void) {
  // a
  simple { str in // b
    // c
    let success = run {
      // d
      completion(str)
      // e
      return true
      // f
    }
    // g
    if !success {
      // h
      completion("bad")
      // i
    }
    // j
  }
  // k
}
// CALL-NON-ASYNC-IN-ASYNC-COMMENT:      func callNonAsyncInAsyncComment() async -> String {
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:   // a
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:   let str = await simple()
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:   // b
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:   // c
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:   return await withCheckedContinuation { continuation in
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:     let success = run {
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:       // d
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:       continuation.resume(returning: str)
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:       // e
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:       {{^}} return true{{$}}
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:       // f
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:     }
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:     // g
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:     if !success {
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:       // h
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:       continuation.resume(returning: "bad")
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:       // i
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:     }
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:     // j
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:     {{ }}
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:     // k
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT:   }
// CALL-NON-ASYNC-IN-ASYNC-COMMENT-NEXT: }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix VOID-AND-ERROR-HANDLER %s
func voidAndErrorCompletion(completion: @escaping (Void?, Error?) -> Void) {
  if .random() {
    completion((), nil) // Make sure we drop the ()
  } else {
    completion(nil, CustomError.Bad)
  }
}
// VOID-AND-ERROR-HANDLER:      func voidAndErrorCompletion() async throws {
// VOID-AND-ERROR-HANDLER-NEXT:   if .random() {
// VOID-AND-ERROR-HANDLER-NEXT:     return // Make sure we drop the ()
// VOID-AND-ERROR-HANDLER-NEXT:   } else {
// VOID-AND-ERROR-HANDLER-NEXT:     throw CustomError.Bad
// VOID-AND-ERROR-HANDLER-NEXT:   }
// VOID-AND-ERROR-HANDLER-NEXT: }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix TOO-MUCH-VOID-AND-ERROR-HANDLER %s
func tooMuchVoidAndErrorCompletion(completion: @escaping (Void?, Void?, Error?) -> Void) {
  if .random() {
    completion((), (), nil) // Make sure we drop the ()s
  } else {
    completion(nil, nil, CustomError.Bad)
  }
}
// TOO-MUCH-VOID-AND-ERROR-HANDLER:      func tooMuchVoidAndErrorCompletion() async throws {
// TOO-MUCH-VOID-AND-ERROR-HANDLER-NEXT:   if .random() {
// TOO-MUCH-VOID-AND-ERROR-HANDLER-NEXT:     return // Make sure we drop the ()s
// TOO-MUCH-VOID-AND-ERROR-HANDLER-NEXT:   } else {
// TOO-MUCH-VOID-AND-ERROR-HANDLER-NEXT:     throw CustomError.Bad
// TOO-MUCH-VOID-AND-ERROR-HANDLER-NEXT:   }
// TOO-MUCH-VOID-AND-ERROR-HANDLER-NEXT: }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix VOID-RESULT-HANDLER %s
func voidResultCompletion(completion: @escaping (Result<Void, Error>) -> Void) {
  if .random() {
    completion(.success(())) // Make sure we drop the .success(())
  } else {
    completion(.failure(CustomError.Bad))
  }
}
// VOID-RESULT-HANDLER:      func voidResultCompletion() async throws {
// VOID-RESULT-HANDLER-NEXT:   if .random() {
// VOID-RESULT-HANDLER-NEXT:     return // Make sure we drop the .success(())
// VOID-RESULT-HANDLER-NEXT:   } else {
// VOID-RESULT-HANDLER-NEXT:     throw CustomError.Bad
// VOID-RESULT-HANDLER-NEXT:   }
// VOID-RESULT-HANDLER-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+2):1 | %FileCheck -check-prefix=NON-COMPLETION-HANDLER %s
// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NON-COMPLETION-HANDLER %s
func functionWithSomeHandler(handler: @escaping (String) -> Void) {}
// NON-COMPLETION-HANDLER: func functionWithSomeHandler() async -> String {}

// rdar://77789360 Make sure we don't print a double return statement.
// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=RETURN-HANDLING %s
func testReturnHandling(_ completion: @escaping (String?, Error?) -> Void) {
  return completion("", nil)
}
// RETURN-HANDLING:      func testReturnHandling() async throws -> String {
// RETURN-HANDLING-NEXT:   {{^}} return ""{{$}}
// RETURN-HANDLING-NEXT: }

// rdar://77789360 Make sure we don't print a double return statement and don't
// completely drop completion(a).
// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=RETURN-HANDLING2 %s
func testReturnHandling2(completion: @escaping (String) -> ()) {
  testReturnHandling { x, err in
    guard let x = x else {
      let a = ""
      return completion(a)
    }
    let b = ""
    return completion(b)
  }
}
// RETURN-HANDLING2:      func testReturnHandling2() async -> String {
// RETURN-HANDLING2-NEXT:   do {
// RETURN-HANDLING2-NEXT:     let x = try await testReturnHandling()
// RETURN-HANDLING2-NEXT:     let b = ""
// RETURN-HANDLING2-NEXT:     {{^}}<#return#> b{{$}}
// RETURN-HANDLING2-NEXT:   } catch let err {
// RETURN-HANDLING2-NEXT:     let a = ""
// RETURN-HANDLING2-NEXT:     {{^}}<#return#> a{{$}}
// RETURN-HANDLING2-NEXT:   }
// RETURN-HANDLING2-NEXT: }

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=RETURN-HANDLING3 %s
func testReturnHandling3(_ completion: @escaping (String?, Error?) -> Void) {
  return (completion("", nil))
}
// RETURN-HANDLING3:      func testReturnHandling3() async throws -> String {
// RETURN-HANDLING3-NEXT:   {{^}} return ""{{$}}
// RETURN-HANDLING3-NEXT: }

// RUN: %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=RETURN-HANDLING4 %s
func testReturnHandling4(_ completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: "xxx") { str, err in
    if str != nil {
      completion(str, err)
      return
    }
    print("some error stuff")
    completion(str, err)
  }
}
// RETURN-HANDLING4:      func testReturnHandling4() async throws -> String {
// RETURN-HANDLING4-NEXT:   do {
// RETURN-HANDLING4-NEXT:     let str = try await simpleErr(arg: "xxx")
// RETURN-HANDLING4-NEXT:     return str
// RETURN-HANDLING4-NEXT:   } catch let err {
// RETURN-HANDLING4-NEXT:     print("some error stuff")
// RETURN-HANDLING4-NEXT:     throw err
// RETURN-HANDLING4-NEXT:   }
// RETURN-HANDLING4-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=RDAR78693050 %s
func rdar78693050(_ completion: @escaping () -> Void) {
  simple { str in
    print(str)
  }
  if .random() {
    return completion()
  }
  completion()
}

// RDAR78693050:      func rdar78693050() async {
// RDAR78693050-NEXT:   let str = await simple()
// RDAR78693050-NEXT:   print(str)
// RDAR78693050-NEXT:   if .random() {
// RDAR78693050-NEXT:     return
// RDAR78693050-NEXT:   }
// RDAR78693050-NEXT:   return
// RDAR78693050-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=DISCARDABLE-RESULT %s
func withDefaultedCompletion(arg: String, completion: @escaping (String) -> Void = {_ in}) {
  completion(arg)
}

// DISCARDABLE-RESULT:      @discardableResult
// DISCARDABLE-RESULT-NEXT: func withDefaultedCompletion(arg: String) async -> String {
// DISCARDABLE-RESULT-NEXT:   return arg
// DISCARDABLE-RESULT-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=DEFAULT-ARG %s
func withDefaultArg(x: String = "") {
}

// DEFAULT-ARG:      convert_function.swift [[# @LINE-3]]:1 -> [[# @LINE-2]]:2
// DEFAULT-ARG-NOT:  @discardableResult
// DEFAULT-ARG-NEXT: {{^}}func withDefaultArg(x: String = "") async

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=IMPLICIT-RETURN %s
func withImplicitReturn(completionHandler: @escaping (String) -> Void) {
  simple {
    completionHandler($0)
  }
}
// IMPLICIT-RETURN: func withImplicitReturn() async -> String {
// IMPLICIT-RETURN-NEXT:   let val0 = await simple()
// IMPLICIT-RETURN-NEXT:   return val0
// IMPLICIT-RETURN-NEXT: }

// This code doesn't compile after refactoring because we can't return `nil` from the async function.
// But there's not much else we can do here.
// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NIL-RESULT-AND-NIL-ERROR %s
func nilResultAndNilError(completion: @escaping (String?, Error?) -> Void) {
  completion(nil, nil)
}
// NIL-RESULT-AND-NIL-ERROR:      func nilResultAndNilError() async throws -> String {
// NIL-RESULT-AND-NIL-ERROR-NEXT:   return nil
// NIL-RESULT-AND-NIL-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NIL-RESULT-AND-OPTIONAL-RELAYED-ERROR %s
func nilResultAndOptionalRelayedError(completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: "test") { (res, err) in
    completion(nil, err)
  }
}
// NIL-RESULT-AND-OPTIONAL-RELAYED-ERROR:      func nilResultAndOptionalRelayedError() async throws -> String {
// NIL-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT:   let res = try await simpleErr(arg: "test")
// NIL-RESULT-AND-OPTIONAL-RELAYED-ERROR-EMPTY:
// NIL-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT: }

// This code doesn't compile after refactoring because we can't throw an optional error returned from makeOptionalError().
// But it's not clear what the intended result should be either.
// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NIL-RESULT-AND-OPTIONAL-COMPLEX-ERROR %s
func nilResultAndOptionalComplexError(completion: @escaping (String?, Error?) -> Void) {
  completion(nil, makeOptionalError())
}
// NIL-RESULT-AND-OPTIONAL-COMPLEX-ERROR:      func nilResultAndOptionalComplexError() async throws -> String {
// NIL-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   throw makeOptionalError()
// NIL-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NIL-RESULT-AND-NON-OPTIONAL-ERROR %s
func nilResultAndNonOptionalError(completion: @escaping (String?, Error?) -> Void) {
  completion(nil, CustomError.Bad)
}
// NIL-RESULT-AND-NON-OPTIONAL-ERROR:      func nilResultAndNonOptionalError() async throws -> String {
// NIL-RESULT-AND-NON-OPTIONAL-ERROR-NEXT:   throw CustomError.Bad
// NIL-RESULT-AND-NON-OPTIONAL-ERROR-NEXT: }

// In this case, we are previously ignoring the error returned from simpleErr but are rethrowing it in the refactored case.
// That's probably fine although it changes semantics.
// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=OPTIONAL-RELAYED-RESULT-AND-NIL-ERROR %s
func optionalRelayedResultAndNilError(completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: "test") { (res, err) in
    completion(res, nil)
  }
}
// OPTIONAL-RELAYED-RESULT-AND-NIL-ERROR:      func optionalRelayedResultAndNilError() async throws -> String {
// OPTIONAL-RELAYED-RESULT-AND-NIL-ERROR-NEXT:   let res = try await simpleErr(arg: "test")
// OPTIONAL-RELAYED-RESULT-AND-NIL-ERROR-NEXT:   return res
// OPTIONAL-RELAYED-RESULT-AND-NIL-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-RELAYED-ERROR %s
func optionalRelayedResultAndOptionalRelayedError(completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: "test") { (res, err) in
    completion(res, err)
  }
}
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-RELAYED-ERROR:      func optionalRelayedResultAndOptionalRelayedError() async throws -> String {
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT:   let res = try await simpleErr(arg: "test")
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT:   return res
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR %s
func optionalRelayedResultAndOptionalComplexError(completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: "test") { (res, err) in
    completion(res, makeOptionalError())
  }
}
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR:      func optionalRelayedResultAndOptionalComplexError() async throws -> String {
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   let res = try await simpleErr(arg: "test")
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   if let error = makeOptionalError() {
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:     throw error
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   } else {
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:     return res
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   }
// OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=OPTIONAL-RELAYED-RESULT-AND-NON-OPTIONAL-ERROR %s
func optionalRelayedResultAndNonOptionalError(completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: "test") { (res, err) in
    completion(res, CustomError.Bad)
  }
}
// OPTIONAL-RELAYED-RESULT-AND-NON-OPTIONAL-ERROR:      func optionalRelayedResultAndNonOptionalError() async throws -> String {
// OPTIONAL-RELAYED-RESULT-AND-NON-OPTIONAL-ERROR-NEXT:   let res = try await simpleErr(arg: "test")
// OPTIONAL-RELAYED-RESULT-AND-NON-OPTIONAL-ERROR-NEXT:   throw CustomError.Bad
// OPTIONAL-RELAYED-RESULT-AND-NON-OPTIONAL-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NON-OPTIONAL-RELAYED-RESULT-AND-NIL-ERROR %s
func nonOptionalRelayedResultAndNilError(completion: @escaping (String?, Error?) -> Void) {
  simple { res in
    completion(res, nil)
  }
}
// NON-OPTIONAL-RELAYED-RESULT-AND-NIL-ERROR:      func nonOptionalRelayedResultAndNilError() async throws -> String {
// NON-OPTIONAL-RELAYED-RESULT-AND-NIL-ERROR-NEXT:   let res = await simple()
// NON-OPTIONAL-RELAYED-RESULT-AND-NIL-ERROR-NEXT:   return res
// NON-OPTIONAL-RELAYED-RESULT-AND-NIL-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NON-OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR %s
func nonOptionalRelayedResultAndOptionalComplexError(completion: @escaping (String?, Error?) -> Void) {
  simple { res in
    completion(res, makeOptionalError())
  }
}
// NON-OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR:      func nonOptionalRelayedResultAndOptionalComplexError() async throws -> String {
// NON-OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   let res = await simple()
// NON-OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   if let error = makeOptionalError() {
// NON-OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:     throw error
// NON-OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   } else {
// NON-OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:     return res
// NON-OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   }
// NON-OPTIONAL-RELAYED-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NON-OPTIONAL-RELAYED-RESULT-AND-NON-OPTIONAL-ERROR %s
func nonOptionalRelayedResultAndNonOptionalError(completion: @escaping (String?, Error?) -> Void) {
  simple { res in
    completion(res, CustomError.Bad)
  }
}
// NON-OPTIONAL-RELAYED-RESULT-AND-NON-OPTIONAL-ERROR:      func nonOptionalRelayedResultAndNonOptionalError() async throws -> String {
// NON-OPTIONAL-RELAYED-RESULT-AND-NON-OPTIONAL-ERROR-NEXT:   let res = await simple()
// NON-OPTIONAL-RELAYED-RESULT-AND-NON-OPTIONAL-ERROR-NEXT:   throw CustomError.Bad
// NON-OPTIONAL-RELAYED-RESULT-AND-NON-OPTIONAL-ERROR-NEXT: }

// The refactored code doesn't compile because we can't return an optional String from the async function. 
// But it's not clear what the intended result should be either, because `error` is always `nil`.
// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=OPTIONAL-COMPLEX-RESULT-AND-NIL-ERROR %s
func optionalComplexResultAndNilError(completion: @escaping (String?, Error?) -> Void) {
  completion(makeOptionalString(), nil)
}
// OPTIONAL-COMPLEX-RESULT-AND-NIL-ERROR:      func optionalComplexResultAndNilError() async throws -> String {
// OPTIONAL-COMPLEX-RESULT-AND-NIL-ERROR-NEXT:   return makeOptionalString()
// OPTIONAL-COMPLEX-RESULT-AND-NIL-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-RELAYED-ERROR %s
func optionalComplexResultAndOptionalRelayedError(completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: "test") { (res, err) in
    completion(makeOptionalString(), err)
  }
}
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-RELAYED-ERROR:      func optionalComplexResultAndOptionalRelayedError() async throws -> String {
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT:   let res = try await simpleErr(arg: "test")
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT:   return makeOptionalString()
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-COMPLEX-ERROR %s
func optionalComplexResultAndOptionalComplexError(completion: @escaping (String?, Error?) -> Void) {
  completion(makeOptionalString(), makeOptionalError())
}
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-COMPLEX-ERROR:      func optionalComplexResultAndOptionalComplexError() async throws -> String {
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   if let error = makeOptionalError() {
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:     throw error
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   } else {
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:     return makeOptionalString()
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   }
// OPTIONAL-COMPLEX-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=OPTIONAL-COMPLEX-RESULT-AND-NON-OPTIONAL-ERROR %s
func optionalComplexResultAndNonOptionalError(completion: @escaping (String?, Error?) -> Void) {
  completion(makeOptionalString(), CustomError.Bad)
}
// OPTIONAL-COMPLEX-RESULT-AND-NON-OPTIONAL-ERROR:      func optionalComplexResultAndNonOptionalError() async throws -> String {
// OPTIONAL-COMPLEX-RESULT-AND-NON-OPTIONAL-ERROR-NEXT:   throw CustomError.Bad
// OPTIONAL-COMPLEX-RESULT-AND-NON-OPTIONAL-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NON-OPTIONAL-RESULT-AND-NIL-ERROR %s
func nonOptionalResultAndNilError(completion: @escaping (String?, Error?) -> Void) {
  completion("abc", nil)
}
// NON-OPTIONAL-RESULT-AND-NIL-ERROR:      func nonOptionalResultAndNilError() async throws -> String {
// NON-OPTIONAL-RESULT-AND-NIL-ERROR-NEXT:   return "abc"
// NON-OPTIONAL-RESULT-AND-NIL-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NON-OPTIONAL-RESULT-AND-OPTIONAL-RELAYED-ERROR %s
func nonOptionalResultAndOptionalRelayedError(completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: "test") { (res, err) in
    completion("abc", err)
  }
}
// NON-OPTIONAL-RESULT-AND-OPTIONAL-RELAYED-ERROR:      func nonOptionalResultAndOptionalRelayedError() async throws -> String {
// NON-OPTIONAL-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT:   let res = try await simpleErr(arg: "test")
// NON-OPTIONAL-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT:   return "abc"
// NON-OPTIONAL-RESULT-AND-OPTIONAL-RELAYED-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NON-OPTIONAL-RESULT-AND-OPTIONAL-COMPLEX-ERROR %s
func nonOptionalResultAndOptionalComplexError(completion: @escaping (String?, Error?) -> Void) {
  completion("abc", makeOptionalError())
}
// NON-OPTIONAL-RESULT-AND-OPTIONAL-COMPLEX-ERROR:      func nonOptionalResultAndOptionalComplexError() async throws -> String {
// NON-OPTIONAL-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   if let error = makeOptionalError() {
// NON-OPTIONAL-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:     throw error
// NON-OPTIONAL-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   } else {
// NON-OPTIONAL-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:     return "abc"
// NON-OPTIONAL-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT:   }
// NON-OPTIONAL-RESULT-AND-OPTIONAL-COMPLEX-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NON-OPTIONAL-RESULT-AND-NON-OPTIONAL-ERROR %s
func nonOptionalResultAndNonOptionalError(completion: @escaping (String?, Error?) -> Void) {
  completion("abc", CustomError.Bad)
}
// NON-OPTIONAL-RESULT-AND-NON-OPTIONAL-ERROR:      func nonOptionalResultAndNonOptionalError() async throws -> String {
// NON-OPTIONAL-RESULT-AND-NON-OPTIONAL-ERROR-NEXT:   throw CustomError.Bad
// NON-OPTIONAL-RESULT-AND-NON-OPTIONAL-ERROR-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=WRAP-COMPLETION-CALL-IN-PARENS %s
func wrapCompletionCallInParenthesis(completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: "test") { (res, err) in
    (completion(res, err))
  }
}
// WRAP-COMPLETION-CALL-IN-PARENS:      func wrapCompletionCallInParenthesis() async throws -> String {
// WRAP-COMPLETION-CALL-IN-PARENS-NEXT:   let res = try await simpleErr(arg: "test")
// WRAP-COMPLETION-CALL-IN-PARENS-NEXT:   return res
// WRAP-COMPLETION-CALL-IN-PARENS-NEXT: }

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=TWO-COMPLETION-HANDLER-CALLS %s
func twoCompletionHandlerCalls(completion: @escaping (String?, Error?) -> Void) {
  simpleErr(arg: "test") { (res, err) in
    completion(res, err)
    completion(res, err)
  }
}
// TWO-COMPLETION-HANDLER-CALLS:      func twoCompletionHandlerCalls() async throws -> String {
// TWO-COMPLETION-HANDLER-CALLS-NEXT:   let res = try await simpleErr(arg: "test")
// TWO-COMPLETION-HANDLER-CALLS-NEXT:   return res
// TWO-COMPLETION-HANDLER-CALLS-NEXT:   return res
// TWO-COMPLETION-HANDLER-CALLS-NEXT: }
