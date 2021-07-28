// RUN: %empty-directory(%t)

func withAsyncAlternative(completionHandler: @escaping (Int) -> Void) {}
func withAsyncAlternative() async -> Int { return 42 }
func withAsyncThrowingAlternative(completionHandler: @escaping (Int?, Error?) -> Void) {}
func withAsyncThrowingAlternative() async throws -> Int { return 42 }

func withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName(closure: @escaping (Int) -> Void) {}
func withoutAsyncAlternativeBecauseOfReturnValue(completionHandler: @escaping (Int) -> Void) -> Bool { return true }
func withoutAsyncAlternativeThrowing(closure: @escaping (Int?, Error?) -> Void) {}
func withoutAsyncAlternativeThrowingWithMultipleResults(closure: @escaping (Int?, String?, Error?) -> Void) {}
func asyncVoidWithoutAlternative(completionHandler2: @escaping () -> Void) {}
func resultWithoutAlternative(completionHandler2: @escaping (Result<Int, Error>) -> Void) {}

func lottaClosures(x: () -> Void, y: () -> Void) -> Int? { nil }

struct MyError: Error {}

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=CREATE-CONTINUATION %s
func testCreateContinuation(completionHandler: @escaping (Int) -> Void) {
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

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=CREATE-CONTINUATION-HANDLER-CALL-IN-PARENS %s
func testCreateContinuationWithCompletionHandlerCallInParens(completionHandler: @escaping (Int) -> Void) {
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
    (completionHandler($0))
  }
}
// CREATE-CONTINUATION-HANDLER-CALL-IN-PARENS:      func testCreateContinuationWithCompletionHandlerCallInParens() async -> Int {
// CREATE-CONTINUATION-HANDLER-CALL-IN-PARENS-NEXT:   return await withCheckedContinuation { continuation in 
// CREATE-CONTINUATION-HANDLER-CALL-IN-PARENS-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName {
// CREATE-CONTINUATION-HANDLER-CALL-IN-PARENS-NEXT:       continuation.resume(returning: $0)
// CREATE-CONTINUATION-HANDLER-CALL-IN-PARENS-NEXT:     }
// CREATE-CONTINUATION-HANDLER-CALL-IN-PARENS-NEXT:   }
// CREATE-CONTINUATION-HANDLER-CALL-IN-PARENS-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=CREATE-CONTINUATION-BECAUSE-RETURN-VALUE %s
func testCreateContinuationBecauseOfReturnValue(completionHandler: @escaping (Int) -> Void) {
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
func testCreateContinuationBecauseOfReturnValue2(completionHandler: @escaping (Int) -> Void) {
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
func testCompletionHandlerCallInNestedExpression(completionHandler: @escaping (Int) -> Void) {
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
func testThrowingContinuation(completionHandler: @escaping (Int?, Error?) -> Void) {
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

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT %s
func testThrowingContinuationRelayingErrorAndResult(completionHandler: @escaping (Int?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theValue, theError) in
    completionHandler(theValue, theError)
  }
}
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT:      func testThrowingContinuationRelayingErrorAndResult() async throws -> Int {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:     withoutAsyncAlternativeThrowing { (theValue, theError) in
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:       if let error = theError {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:         continuation.resume(throwing: error)
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:       } else {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:         guard let theValue1 = theValue else {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:           fatalError("Expected non-nil result 'theValue1' in the non-error case")
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:         }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:         continuation.resume(returning: theValue1)
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:       }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:     }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT:   }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-RESULT-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT %s
func testThrowingContinuationRelayingErrorAndComplexResult(completionHandler: @escaping (Int?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theValue, theError) in
    completionHandler(theValue.map({ $0 + 1 }), theError)
  }
}
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT:      func testThrowingContinuationRelayingErrorAndComplexResult() async throws -> Int {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:     withoutAsyncAlternativeThrowing { (theValue, theError) in
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:       if let error = theError {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:         continuation.resume(throwing: error)
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:       } else {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:         guard let result = theValue.map({ $0 + 1 }) else {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:           fatalError("Expected non-nil result in the non-error case")
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:         }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:         continuation.resume(returning: result)
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:       }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:     }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT:   }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS %s
func testThrowingContinuationRelayingErrorAndTwoComplexResults(completionHandler: @escaping (Int?, Int?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theValue, theError) in
    completionHandler(theValue.map({ $0 + 1 }), theValue.map({ $0 + 2 }), theError)
  }
}
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS:      func testThrowingContinuationRelayingErrorAndTwoComplexResults() async throws -> (Int, Int) {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:     withoutAsyncAlternativeThrowing { (theValue, theError) in
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:       if let error = theError {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:         continuation.resume(throwing: error)
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:       } else {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:         guard let result0 = theValue.map({ $0 + 1 }) else {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:           fatalError("Expected non-nil result 'result0' in the non-error case")
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:         }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:         guard let result1 = theValue.map({ $0 + 2 }) else {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:           fatalError("Expected non-nil result 'result1' in the non-error case")
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:         }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:         continuation.resume(returning: (result0, result1))
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:       }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:     }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT:   }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-TWO-COMPLEX-RESULTS-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE %s
func testThrowingContinuationRelayingErrorAndComplexResultWithTrailingClosure(completionHandler: @escaping (Int?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theValue, theError) in
    completionHandler(theValue.map { $0 + 1 }, theError)
  }
}
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE:      func testThrowingContinuationRelayingErrorAndComplexResultWithTrailingClosure() async throws -> Int {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:     withoutAsyncAlternativeThrowing { (theValue, theError) in
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:       if let error = theError {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:         continuation.resume(throwing: error)
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:       } else {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:         guard let result = (theValue.map { $0 + 1 }) else {
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:           fatalError("Expected non-nil result in the non-error case")
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:         }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:         continuation.resume(returning: result)
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:       }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:     }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT:   }
// THROWING-CONTINUATION-RELAYING-ERROR-AND-COMPLEX-RESULT-WITH-TRAILING-CLOSURE-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=MULTIPLE-TRAILING-CLOSURES %s
func testThrowingContinuationRelayingErrorAndComplexResultWithMultipleTrailingClosures(completionHandler: @escaping (Int?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { theValue, theError in
    completionHandler(lottaClosures {} y: {}, theError)
  }
}
// MULTIPLE-TRAILING-CLOSURES:      func testThrowingContinuationRelayingErrorAndComplexResultWithMultipleTrailingClosures() async throws -> Int {
// MULTIPLE-TRAILING-CLOSURES-NEXT:   return try await withCheckedThrowingContinuation { continuation in
// MULTIPLE-TRAILING-CLOSURES-NEXT:     withoutAsyncAlternativeThrowing { theValue, theError in
// MULTIPLE-TRAILING-CLOSURES-NEXT:       if let error = theError {
// MULTIPLE-TRAILING-CLOSURES-NEXT:         continuation.resume(throwing: error)
// MULTIPLE-TRAILING-CLOSURES-NEXT:       } else {
// MULTIPLE-TRAILING-CLOSURES-NEXT:         guard let result = (lottaClosures {} y: {}) else {
// MULTIPLE-TRAILING-CLOSURES-NEXT:           fatalError("Expected non-nil result in the non-error case")
// MULTIPLE-TRAILING-CLOSURES-NEXT:         }
// MULTIPLE-TRAILING-CLOSURES-NEXT:         continuation.resume(returning: result)
// MULTIPLE-TRAILING-CLOSURES-NEXT:       }
// MULTIPLE-TRAILING-CLOSURES-NEXT:     }
// MULTIPLE-TRAILING-CLOSURES-NEXT:   }
// MULTIPLE-TRAILING-CLOSURES-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=THROWING-CONTINUATION-ALWAYS-RETURNING-ERROR-AND-RESULT %s
func testAlwaysReturnBothResultAndCompletionHandler(completionHandler: @escaping (Int?, Error?) -> Void) {
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { theValue in
    completionHandler(theValue, MyError())
  }
}
// THROWING-CONTINUATION-ALWAYS-RETURNING-ERROR-AND-RESULT:      func testAlwaysReturnBothResultAndCompletionHandler() async throws -> Int {
// THROWING-CONTINUATION-ALWAYS-RETURNING-ERROR-AND-RESULT-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// THROWING-CONTINUATION-ALWAYS-RETURNING-ERROR-AND-RESULT-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { theValue in
// THROWING-CONTINUATION-ALWAYS-RETURNING-ERROR-AND-RESULT-NEXT:       continuation.resume(throwing: MyError())
// THROWING-CONTINUATION-ALWAYS-RETURNING-ERROR-AND-RESULT-NEXT:     }
// THROWING-CONTINUATION-ALWAYS-RETURNING-ERROR-AND-RESULT-NEXT:   }
// THROWING-CONTINUATION-ALWAYS-RETURNING-ERROR-AND-RESULT-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE %s
func testAmbiguousCallToCompletionHandlerWithAlwaysNilVariable(completionHandler: @escaping (Int?, Error?) -> Void) {
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { theValue in
    let error: Error? = nil
    completionHandler(theValue, error)
  }
}
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE:      func testAmbiguousCallToCompletionHandlerWithAlwaysNilVariable() async throws -> Int {
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { theValue in
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT:       let error: Error? = nil
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT:       if let error = error {
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT:         continuation.resume(throwing: error)
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT:       } else {
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT:         continuation.resume(returning: theValue)
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT:       }
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT:     }
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT:   }
// AMBIGUOUS-CALL-WITH-ALWAYS-NIL-VARIABLE-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=PREVIOUS-COMPLETION-HANDLER-CALL %s
func testPreviousCompletionHandlerCall(completionHandler: @escaping (Int) -> Void) {
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
func testPreviousAsyncCall(completionHandler: @escaping (Int) -> Void) {
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
func testInIfElse(completionHandler: @escaping (Int) -> Void) {
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
func testAsyncAfterContinuation(completionHandler: @escaping (Int) -> Void) {
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
func testWithoutAsyncAlternativeNestedInWithoutAsyncAlternative(completionHandler: @escaping (Int) -> Void) {
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
func testWithoutAsyncAlternativeNestedInAsyncAlternative(completionHandler: @escaping (Int) -> Void) {
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
func testAsyncAlternativeNestedInWithoutAsyncAlternative(completionHandler: @escaping (Int) -> Void) {
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
func testShadowContName(completionHandler: @escaping (Int) -> Void) {
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
func testShadowContName2(completionHandler: @escaping (Int) -> Void) {
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
func testVoidReturnValue(completionHandler: @escaping () -> Void) {
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
func testSimpleResult(completionHandler: @escaping (Result<Int, Error>) -> Void) {
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
func testResultFromValueAndError(completionHandler: @escaping (Result<Int, Error>) -> Void) {
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

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=MULTIPLE-RETURN-VALUES-AND-ERROR %s
func testMultipleReturnValuesAndError(completion: @escaping (Int?, String?, Error?) -> Void) {
  withoutAsyncAlternativeThrowingWithMultipleResults { (first, second, error) in 
    completion(first, second, error)
  }
}
// MULTIPLE-RETURN-VALUES-AND-ERROR:      func testMultipleReturnValuesAndError() async throws -> (Int, String) {
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:     withoutAsyncAlternativeThrowingWithMultipleResults { (first, second, error) in 
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:       if let error = error {
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:         continuation.resume(throwing: error)
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:       } else {
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:         guard let first1 = first else {
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:           fatalError("Expected non-nil result 'first1' in the non-error case")
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:         }
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:         guard let second1 = second else {
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:           fatalError("Expected non-nil result 'second1' in the non-error case")
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:         }
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:         continuation.resume(returning: (first1, second1))
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:       }
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:     }
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT:   }
// MULTIPLE-RETURN-VALUES-AND-ERROR-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NON-OPTIONAL-VALUE-FOR-RESULT-AND-ERROR %s
func testReturnNonOptionalValuesForResultAndError(completion: @escaping (Int?, Error?) -> Void) {
  withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { result in
    completion(1, MyError())
  }
}
// NON-OPTIONAL-VALUE-FOR-RESULT-AND-ERROR:      func testReturnNonOptionalValuesForResultAndError() async throws -> Int {
// NON-OPTIONAL-VALUE-FOR-RESULT-AND-ERROR-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// NON-OPTIONAL-VALUE-FOR-RESULT-AND-ERROR-NEXT:     withoutAsyncAlternativeBecauseOfMismatchedCompletionHandlerName { result in
// NON-OPTIONAL-VALUE-FOR-RESULT-AND-ERROR-NEXT:       continuation.resume(throwing: MyError())
// NON-OPTIONAL-VALUE-FOR-RESULT-AND-ERROR-NEXT:     }
// NON-OPTIONAL-VALUE-FOR-RESULT-AND-ERROR-NEXT:   }
// NON-OPTIONAL-VALUE-FOR-RESULT-AND-ERROR-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT %s
func testMixedOptionalAnNonOptionaResults(completion: @escaping (Int?, String?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theResult, error) in
    completion(theResult, "hi", nil)
  }
}
// MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT:      func testMixedOptionalAnNonOptionaResults() async throws -> (Int, String) {
// MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT-NEXT:     withoutAsyncAlternativeThrowing { (theResult, error) in
// MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT-NEXT:       guard let theResult1 = theResult else {
// MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT-NEXT:         fatalError("Expected non-nil result 'theResult1' in the non-error case")
// MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT-NEXT:       }
// MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT-NEXT:       continuation.resume(returning: (theResult1, "hi"))
// MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT-NEXT:     }
// MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT-NEXT:   }
// MIXED-OPTIONAL-AND-NON-OPTIONAL-RESULT-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL %s
func testUseOptionalResultValueAfterCompletionHandlerCall(completion: @escaping (Int?, String?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theResult, error) in
    completion(theResult, "hi", nil)
    print(theResult.map { $0 + 1 } as Any)
  }
}
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL:      func testUseOptionalResultValueAfterCompletionHandlerCall() async throws -> (Int, String) {
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL-NEXT:     withoutAsyncAlternativeThrowing { (theResult, error) in
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL-NEXT:       guard let theResult1 = theResult else {
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL-NEXT:         fatalError("Expected non-nil result 'theResult1' in the non-error case")
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL-NEXT:       }
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL-NEXT:       continuation.resume(returning: (theResult1, "hi"))
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL-NEXT:       print(theResult.map { $0 + 1 } as Any)
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL-NEXT:     }
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL-NEXT:   }
// USE-OPTIONAL-RESULT-AFTER-COMPLETION-HANDLER-CALL-NEXT: }

// We shouldn't need to unwrap `theResult` twice here, but the example is silly and I don't care too much.
// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=PASS-SAME-RESULT-TWICE %s
func testPassSameResultTwice(completion: @escaping (Int?, Int?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theResult, error) in
    completion(theResult, theResult, nil)
  }
}
// PASS-SAME-RESULT-TWICE:      func testPassSameResultTwice() async throws -> (Int, Int) {
// PASS-SAME-RESULT-TWICE-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// PASS-SAME-RESULT-TWICE-NEXT:     withoutAsyncAlternativeThrowing { (theResult, error) in
// PASS-SAME-RESULT-TWICE-NEXT:       guard let theResult1 = theResult else {
// PASS-SAME-RESULT-TWICE-NEXT:         fatalError("Expected non-nil result 'theResult1' in the non-error case")
// PASS-SAME-RESULT-TWICE-NEXT:       }
// PASS-SAME-RESULT-TWICE-NEXT:       guard let theResult2 = theResult else {
// PASS-SAME-RESULT-TWICE-NEXT:         fatalError("Expected non-nil result 'theResult2' in the non-error case")
// PASS-SAME-RESULT-TWICE-NEXT:       }
// PASS-SAME-RESULT-TWICE-NEXT:       continuation.resume(returning: (theResult1, theResult2))
// PASS-SAME-RESULT-TWICE-NEXT:     }
// PASS-SAME-RESULT-TWICE-NEXT:   }
// PASS-SAME-RESULT-TWICE-NEXT: }


// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL %s
func testUseResultAfterAmbiguousCompletionHandlerCall(completion: @escaping (Int?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theResult, error) in
    completion(theResult, error)
    print(theResult as Any)
  }
}
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL:      func testUseResultAfterAmbiguousCompletionHandlerCall() async throws -> Int {
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:     withoutAsyncAlternativeThrowing { (theResult, error) in
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:       if let error = error {
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:         continuation.resume(throwing: error)
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:       } else {
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:         guard let theResult1 = theResult else {
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:           fatalError("Expected non-nil result 'theResult1' in the non-error case")
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:         }
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:         continuation.resume(returning: theResult1)
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:       }
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:       print(theResult as Any)
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:     }
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT:   }
// USE-RESULT-AFTER-AMBIGUOUS-HANLDER-CALL-NEXT: }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=TWO-COMPLEITON-HANDLER-CALLS %s
func testTwoCompletionHandlerCalls(completion: @escaping (Int?, Error?) -> Void) {
  withoutAsyncAlternativeThrowing { (theResult, error) in
    completion(theResult, error)
    completion(theResult, error)
  }
}
// TWO-COMPLEITON-HANDLER-CALLS:      func testTwoCompletionHandlerCalls() async throws -> Int {
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:     withoutAsyncAlternativeThrowing { (theResult, error) in
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:       if let error = error {
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:         continuation.resume(throwing: error)
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:       } else {
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:         guard let theResult1 = theResult else {
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:           fatalError("Expected non-nil result 'theResult1' in the non-error case")
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:         }
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:         continuation.resume(returning: theResult1)
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:       }
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:       if let error = error {
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:         continuation.resume(throwing: error)
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:       } else {
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:         guard let theResult2 = theResult else {
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:           fatalError("Expected non-nil result 'theResult2' in the non-error case")
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:         }
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:         continuation.resume(returning: theResult2)
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:       }
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:     }
// TWO-COMPLEITON-HANDLER-CALLS-NEXT:   }
// TWO-COMPLEITON-HANDLER-CALLS-NEXT: }


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
// URL-SESSION:      func testDataTask() async -> Int? {
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
