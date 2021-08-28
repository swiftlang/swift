// REQUIRES: concurrency

// RUN: %empty-directory(%t)

func callbackIntWithError(_ completion: @escaping (Bool, Error?) -> Void) {}

// rdar://79864182
// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=INVALID-COND %s
callbackIntWithError { x, err in
  if x {
    print("ok")
  }
}
// INVALID-COND:      let x = try await callbackIntWithError()
// INVALID-COND-NEXT: if x {
// INVALID-COND-NEXT:   print("ok")
// INVALID-COND-NEXT: }

func withoutAsyncAlternative(closure: (Int) -> Void) {}

// RUN: %refactor -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=UNKNOWN-ERROR-IN-CONTINUATION %s
func testUnknownErrorInContinuation(completionHandler: @escaping (Int?, Error?) -> Void) {
  withoutAsyncAlternative { theValue in
    completionHandler(theValue, MyUndefinedError())
  }
}
// UNKNOWN-ERROR-IN-CONTINUATION:      func testUnknownErrorInContinuation() async throws -> Int {
// UNKNOWN-ERROR-IN-CONTINUATION-NEXT:   return try await withCheckedThrowingContinuation { continuation in 
// UNKNOWN-ERROR-IN-CONTINUATION-NEXT:     withoutAsyncAlternative { theValue in
// UNKNOWN-ERROR-IN-CONTINUATION-NEXT:       continuation.resume(throwing: MyUndefinedError())
// UNKNOWN-ERROR-IN-CONTINUATION-NEXT:     }
// UNKNOWN-ERROR-IN-CONTINUATION-NEXT:   }
// UNKNOWN-ERROR-IN-CONTINUATION-NEXT: }
