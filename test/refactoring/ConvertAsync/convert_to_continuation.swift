// RUN: %empty-directory(%t)

func withAsyncAlternative(completionHandler: (Int) -> Void) {}
func withAsyncAlternative() async -> Int { return 42 }
func withAsyncThrowingAlternative(completionHandler: (Int?, Error?) -> Void) {}
func withAsyncThrowingAlternative() async throws -> Int { return 42 }

func withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName(closure: (Int) -> Void) {}
func withoutAsyncAlternativeBecauseOfReturnValue(completionHandler: (Int) -> Void) -> Bool { return true }
func withoutAsyncAlternativeThrowing(closure: (Int?, Error?) -> Void) {}
func asyncVoidWithoutAlternative(completionHandler2: () -> Void) {}
func resultWithoutAlternative(completionHandler2: (Result<Int, Error>) -> Void) {}


// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=CREATE-CONTINUATION %s
func testCreateContinuation(completionHandler: (Int) -> Void) {
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
    completionHandler($0)
  }
}
// CREATE-CONTINUATION:      func testCreateContinuation() async -> Int {
// CREATE-CONTINUATION-NEXT:   return await withCheckedContinuation { continuation in 
// CREATE-CONTINUATION-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
// CREATE-CONTINUATION-NEXT:       continuation.resume(returning: $0)
// CREATE-CONTINUATION-NEXT:     }
// CREATE-CONTINUATION-NEXT:   }
// CREATE-CONTINUATION-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=CREATE-CONTINUATION-BECAUSE-RETURN-VALUE %s
func testCreateContinuationBecauseOfReturnValue(completionHandler: (Int) -> Void) {
  _ = withoutAsyncAlternativeBecauseOfReturnValue {
    completionHandler($0)
  }
}
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE:      func testCreateContinuationBecauseOfReturnValue() async -> Int {
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-NEXT:   return await withCheckedContinuation { continuation in 
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-NEXT:     _ = withoutAsyncAlternativeBecauseOfReturnValue {
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-NEXT:       continuation.resume(returning: $0)
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-NEXT:     }
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-NEXT:   }
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-2 %s
func testCreateContinuationBecauseOfReturnValue2(completionHandler: (Int) -> Void) {
  let x = withoutAsyncAlternativeBecauseOfReturnValue {
    completionHandler($0)
  }
  print(x)
}
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-2:      func testCreateContinuationBecauseOfReturnValue2() async -> Int {
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-2-NEXT:   return await withCheckedContinuation { continuation in 
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-2-NEXT:     let x = withoutAsyncAlternativeBecauseOfReturnValue {
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-2-NEXT:       continuation.resume(returning: $0)
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-2-NEXT:     }
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-2-NEXT:     print(x)
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-2-NEXT:   }
// CREATE-CONTINUATION-BECAUSE-RETURN-VALUE-2-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=CONTINUATION-IN-NESTED-EXPRESSION %s
func testCompletionHandlerCallInNestedExpression(completionHandler: (Int) -> Void) {
  print(withoutAsyncAlternativeBecauseOfReturnValue {
    completionHandler($0)
  })
}
// CONTINUATION-IN-NESTED-EXPRESSION:      func testCompletionHandlerCallInNestedExpression() async -> Int {
// CONTINUATION-IN-NESTED-EXPRESSION-NEXT:   return await withCheckedContinuation { continuation in 
// CONTINUATION-IN-NESTED-EXPRESSION-NEXT:     print(withoutAsyncAlternativeBecauseOfReturnValue {
// CONTINUATION-IN-NESTED-EXPRESSION-NEXT:       continuation.resume(returning: $0)
// CONTINUATION-IN-NESTED-EXPRESSION-NEXT:     })
// CONTINUATION-IN-NESTED-EXPRESSION-NEXT:   }
// CONTINUATION-IN-NESTED-EXPRESSION-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=THROWING-CONTINUATION %s
func testThrowingContinuation(completionHandler: (Int?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theValue, theError) in
    if let theError = theError {
      completionHandler(nil, theError)
    } else {
      completionHandler(theValue!, nil)
    }
  }
}
// THROWING-CONTINUATION:      func testThrowingContinuation() async throws -> Int {
// THROWING-CONTINUATION-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// THROWING-CONTINUATION-NEXT:     withoutAsyncAlternativeThrowing { (theValue, theError) in
// THROWING-CONTINUATION-NEXT:     if let theError = theError {
// THROWING-CONTINUATION-NEXT:       continuation.resume(throwing: theError)
// THROWING-CONTINUATION-NEXT:     } else {
// THROWING-CONTINUATION-NEXT:       continuation.resume(returning: theValue!)
// THROWING-CONTINUATION-NEXT:     }
// THROWING-CONTINUATION-NEXT:   }
// THROWING-CONTINUATION-NEXT: }

// We can't relay both the result and the error through the continuation. Converting the following results in a compiler error complaining that theError (of type Error?) can't be passed to `continuation.resume(throwing)`.
// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT %s
func testThrowingContinuationRelayingErrorAndResult(completionHandler: (Int?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theValue, theError) in
    completionHandler(theValue, theError)
  }
}
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT:      func testThrowingContinuationRelayingErrorAndResult() async throws -> Int {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:     withoutAsyncAlternativeThrowing { (theValue, theError) in
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:       continuation.resume(throwing: theError)
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:     }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:   }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT: }


// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=PREVIOUS-COMPLETION-HANDLER-CALL %s
func testPreviousCompletionHandlerCall(completionHandler: (Int) -> Void) {
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
    print($0)
  }
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
    completionHandler($0)
  }
}
// PREVIOUS-COMPLETION-HANDLER-CALL:      func testPreviousCompletionHandlerCall() async -> Int {
// PREVIOUS-COMPLETION-HANDLER-CALL-NEXT:   withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
// PREVIOUS-COMPLETION-HANDLER-CALL-NEXT:     print($0)
// PREVIOUS-COMPLETION-HANDLER-CALL-NEXT:   }
// PREVIOUS-COMPLETION-HANDLER-CALL-NEXT:   return await withCheckedContinuation { continuation in 
// PREVIOUS-COMPLETION-HANDLER-CALL-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
// PREVIOUS-COMPLETION-HANDLER-CALL-NEXT:       continuation.resume(returning: $0)
// PREVIOUS-COMPLETION-HANDLER-CALL-NEXT:     }
// PREVIOUS-COMPLETION-HANDLER-CALL-NEXT:   }
// PREVIOUS-COMPLETION-HANDLER-CALL-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=PREVIOUS-ASYNC-CALL %s
func testPreviousAsyncCall(completionHandler: (Int) -> Void) {
  withAsyncAlternative { message in
    print(message)
  }
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
    completionHandler($0)
  }
}
// PREVIOUS-ASYNC-CALL:      func testPreviousAsyncCall() async -> Int {
// PREVIOUS-ASYNC-CALL-NEXT:   let message = await withAsyncAlternative()
// PREVIOUS-ASYNC-CALL-NEXT:   print(message)
// PREVIOUS-ASYNC-CALL-NEXT:   return await withCheckedContinuation { continuation in 
// PREVIOUS-ASYNC-CALL-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
// PREVIOUS-ASYNC-CALL-NEXT:       continuation.resume(returning: $0)
// PREVIOUS-ASYNC-CALL-NEXT:     }
// PREVIOUS-ASYNC-CALL-NEXT:   }
// PREVIOUS-ASYNC-CALL-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=IN-IF-ELSE %s
func testInIfElse(completionHandler: (Int) -> Void) {
  if true {
    withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
      completionHandler($0)
    }
  } else {
    withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
      completionHandler($0)
    }
  }
}
// IN-IF-ELSE:      func testInIfElse() async -> Int {
// IN-IF-ELSE-NEXT:   if true {
// IN-IF-ELSE-NEXT:     return await withCheckedContinuation { continuation in 
// IN-IF-ELSE-NEXT:       withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
// IN-IF-ELSE-NEXT:         continuation.resume(returning: $0)
// IN-IF-ELSE-NEXT:       }
// IN-IF-ELSE-NEXT:     }
// IN-IF-ELSE-NEXT:   } else {
// IN-IF-ELSE-NEXT:     return await withCheckedContinuation { continuation in 
// IN-IF-ELSE-NEXT:       withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
// IN-IF-ELSE-NEXT:         continuation.resume(returning: $0)
// IN-IF-ELSE-NEXT:       }
// IN-IF-ELSE-NEXT:     }
// IN-IF-ELSE-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-AFTER-CONTINUATION %s
func testAsyncAfterContinuation(completionHandler: (Int) -> Void) {
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
    completionHandler($0)
  }
  withAsyncAlternative {
    print($0)
  }
}
// ASYNC-AFTER-CONTINUATION:      func testAsyncAfterContinuation() async -> Int {
// ASYNC-AFTER-CONTINUATION-NEXT:   return await withCheckedContinuation { continuation in 
// ASYNC-AFTER-CONTINUATION-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
// ASYNC-AFTER-CONTINUATION-NEXT:       continuation.resume(returning: $0)
// ASYNC-AFTER-CONTINUATION-NEXT:     }
// ASYNC-AFTER-CONTINUATION-NEXT:     withAsyncAlternative {
// ASYNC-AFTER-CONTINUATION-NEXT:       print($0)
// ASYNC-AFTER-CONTINUATION-NEXT:     }
// ASYNC-AFTER-CONTINUATION-NEXT:   }
// ASYNC-AFTER-CONTINUATION-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=WITHOUT-ASYNC-NESTED-IN-WITHOUT-ASYNC %s
func testWithoutAsyncAlternativeNestedInWithoutAsyncAlternative(completionHandler: (Int) -> Void) {
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { firstResult in
    withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { secondResult in
      completionHandler(firstResult + secondResult)
    }
  }
}
// WITHOUT-ASYNC-NESTED-IN-WITHOUT-ASYNC:      func testWithoutAsyncAlternativeNestedInWithoutAsyncAlternative() async -> Int {
// WITHOUT-ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:   return await withCheckedContinuation { continuation in 
// WITHOUT-ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { firstResult in
// WITHOUT-ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:       withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { secondResult in
// WITHOUT-ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:         continuation.resume(returning: firstResult + secondResult)
// WITHOUT-ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:       }
// WITHOUT-ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:     }
// WITHOUT-ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:   }
// WITHOUT-ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT: }


// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=WITHOUT-ASYNC-NESTED-IN-ASYNC %s
func testWithoutAsyncAlternativeNestedInAsyncAlternative(completionHandler: (Int) -> Void) {
  withAsyncAlternative { firstResult in
    withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { secondResult in
      completionHandler(firstResult + secondResult)
    }
  }
}
// WITHOUT-ASYNC-NESTED-IN-ASYNC:       func testWithoutAsyncAlternativeNestedInAsyncAlternative() async -> Int {
// WITHOUT-ASYNC-NESTED-IN-ASYNC-NEXT:   let firstResult = await withAsyncAlternative()
// WITHOUT-ASYNC-NESTED-IN-ASYNC-NEXT:   return await withCheckedContinuation { continuation in 
// WITHOUT-ASYNC-NESTED-IN-ASYNC-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { secondResult in
// WITHOUT-ASYNC-NESTED-IN-ASYNC-NEXT:       continuation.resume(returning: firstResult + secondResult)
// WITHOUT-ASYNC-NESTED-IN-ASYNC-NEXT:     }
// WITHOUT-ASYNC-NESTED-IN-ASYNC-NEXT:   }
// WITHOUT-ASYNC-NESTED-IN-ASYNC-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=ASYNC-NESTED-IN-WITHOUT-ASYNC %s
func testAsyncAlternativeNestedInWithoutAsyncAlternative(completionHandler: (Int) -> Void) {
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { firstResult in
    withAsyncAlternative { secondResult in
      completionHandler(firstResult + secondResult)
    }
  }
}
// ASYNC-NESTED-IN-WITHOUT-ASYNC:      func testAsyncAlternativeNestedInWithoutAsyncAlternative() async -> Int {
// ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:   return await withCheckedContinuation { continuation in 
// ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { firstResult in
// ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:       withAsyncAlternative { secondResult in
// ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:         continuation.resume(returning: firstResult + secondResult)
// ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:       }
// ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:     }
// ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT:   }
// ASYNC-NESTED-IN-WITHOUT-ASYNC-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=SHADOW-CONT-NAME %s
func testShadowContName(completionHandler: (Int) -> Void) {
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { continuation in
    completionHandler(continuation)
  }
}
// SHADOW-CONT-NAME:      func testShadowContName() async -> Int {
// SHADOW-CONT-NAME-NEXT:   return await withCheckedContinuation { continuation in 
// SHADOW-CONT-NAME-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { continuation1 in
// SHADOW-CONT-NAME-NEXT:       continuation.resume(returning: continuation1)
// SHADOW-CONT-NAME-NEXT:     }
// SHADOW-CONT-NAME-NEXT:   }
// SHADOW-CONT-NAME-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=SHADOW-CONT-NAME-2 %s
func testShadowContName2(completionHandler: (Int) -> Void) {
  let continuation = 3
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { result in
    completionHandler(result + continuation)
  }
}
// SHADOW-CONT-NAME-2:      func testShadowContName2() async -> Int {
// SHADOW-CONT-NAME-2-NEXT:   let continuation = 3
// SHADOW-CONT-NAME-2-NEXT:   return await withCheckedContinuation { continuation1 in 
// SHADOW-CONT-NAME-2-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { result in
// SHADOW-CONT-NAME-2-NEXT:       continuation1.resume(returning: result + continuation)
// SHADOW-CONT-NAME-2-NEXT:     }
// SHADOW-CONT-NAME-2-NEXT:   }
// SHADOW-CONT-NAME-2-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=VOID-RETURN %s
func testVoidReturnValue(completionHandler: () -> Void) {
  asyncVoidWithoutAlternative {
    completionHandler()
  }
}
// VOID-RETURN:      func testVoidReturnValue() async {
// VOID-RETURN-NEXT:   return await withCheckedContinuation { continuation in 
// VOID-RETURN-NEXT:     asyncVoidWithoutAlternative {
// VOID-RETURN-NEXT:       continuation.resume(returning: ())
// VOID-RETURN-NEXT:     }
// VOID-RETURN-NEXT:   }
// VOID-RETURN-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=SIMPLE-RESULT %s
func testSimpleResult(completionHandler: (Result<Int, Error>) -> Void) {
  resultWithoutAlternative { result in
    completionHandler(result)
  }
}
// SIMPLE-RESULT:      func testSimpleResult() async throws -> Int {
// SIMPLE-RESULT-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// SIMPLE-RESULT-NEXT:     resultWithoutAlternative { result in
// SIMPLE-RESULT-NEXT:       continuation.resume(with: result)
// SIMPLE-RESULT-NEXT:     }
// SIMPLE-RESULT-NEXT:   }
// SIMPLE-RESULT-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=RESULT-FROM-VALUE-AND-ERROR %s
func testResultFromValueAndError(completionHandler: (Result<Int, Error>) -> Void) {
  withoutAsyncAlternativeThrowing { (value, error) in
    if let error = error {
      completionHandler(.failure(error))
    } else {
      completionHandler(.success(value!))
    }
  }
}
// RESULT-FROM-VALUE-AND-ERROR:      func testResultFromValueAndError() async throws -> Int {
// RESULT-FROM-VALUE-AND-ERROR-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// RESULT-FROM-VALUE-AND-ERROR-NEXT:     withoutAsyncAlternativeThrowing { (value, error) in
// RESULT-FROM-VALUE-AND-ERROR-NEXT:       if let error = error {
// RESULT-FROM-VALUE-AND-ERROR-NEXT:         continuation.resume(with: .failure(error))
// RESULT-FROM-VALUE-AND-ERROR-NEXT:       } else {
// RESULT-FROM-VALUE-AND-ERROR-NEXT:         continuation.resume(with: .success(value!))
// RESULT-FROM-VALUE-AND-ERROR-NEXT:       }
// RESULT-FROM-VALUE-AND-ERROR-NEXT:     }
// RESULT-FROM-VALUE-AND-ERROR-NEXT:   }
// RESULT-FROM-VALUE-AND-ERROR-NEXT: }



// Reduced version of https://twitter.com/peterfriese/status/1397835146133479428

class DataTask {
  let completionHandler: (String?, Error?) -> Void

  init(completionHandler: @escaping (String?, Error?) -> Void) {
    self.completionHandler = completionHandler
  }
  func resume() {
    completionHandler("mock result", nil)
  }
}

class URLSession {
  static let shared = URLSession()

  func dataTask(completionHandler: @escaping (String?, Error?) -> Void) -> DataTask {
    return DataTask(completionHandler: completionHandler)
  }
}

func processURLResult(_ data: String) throws -> Int {
  return data.count
}

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=URL-SESSION %s
func testDataTask(_ completion: @escaping (Int?) -> Void) {
  let dataTask = URLSession.shared.dataTask { (data, error) in
    guard let data = data else {
      return // Yes, there is a bug about not calling completion here, but that's copied from the Twitter link above
    }
    do {
      let processed = try processURLResult(data)
      completion(processed)
    } catch {
      completion(nil)
    }
  }
  dataTask.resume()
}
// URL-SESSION:      func testDataTask() async -> Int?
// URL-SESSION-NEXT:   return await withCheckedContinuation { continuation in
// URL-SESSION-NEXT:     let dataTask1 = URLSession.shared.dataTask { (data, error) in
// URL-SESSION-NEXT:       guard let data1 = data else {
// URL-SESSION-NEXT:         return // Yes, there is a bug about not calling completion here, but that's copied from the Twitter link above
// URL-SESSION-NEXT:       }
// URL-SESSION-NEXT:       do {
// URL-SESSION-NEXT:         let processed = try processURLResult(data1)
// URL-SESSION-NEXT:         continuation.resume(returning: processed)
// URL-SESSION-NEXT:       } catch {
// URL-SESSION-NEXT:         continuation.resume(returning: nil)
// URL-SESSION-NEXT:       }
// URL-SESSION-NEXT:     }
// URL-SESSION-NEXT:     dataTask1.resume()
// URL-SESSION-NEXT:   }
// URL-SESSION-NEXT: }



// Reduced version of rdar://79304583

class DispatchQueue {
  init() {}

  func async(execute work: @escaping @convention(block) () -> Void) {
    work()
  }
}

func syncComputation() -> Int { return 42 }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=DISPATCH %s
func testDispatch(_ completionHandler: @escaping (Int) -> Void) {
  let queue = DispatchQueue()
  queue.async {
    completionHandler(syncComputation())
  }
}
// DISPATCH: func testDispatch() async -> Int {
// DISPATCH-NEXT:   let queue = DispatchQueue()
// DISPATCH-NEXT:   return await withCheckedContinuation { continuation in
// DISPATCH-NEXT:     queue.async {
// DISPATCH-NEXT:       continuation.resume(returning: syncComputation())
// DISPATCH-NEXT:     }
// DISPATCH-NEXT:   }
// DISPATCH-NEXT: }
