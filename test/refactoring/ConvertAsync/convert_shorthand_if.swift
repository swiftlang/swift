// REQUIRES: concurrency

// RUN: %empty-directory(%t)

func foo(_ fn: @escaping (String, Error?) -> Void) {}
func foo() async throws -> String { return "" }

// RUN: %refactor-check-compiles -convert-to-async -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck %s
func shorthandIf(completion: @escaping (String?, Error?) -> Void) {
  foo { str, error in
    if let error {
      completion(nil, error)
    } else {
      completion(str, nil)
    }
  }
}
// CHECK:      func shorthandIf() async throws -> String {
// CHECK-NEXT:   return try await withCheckedThrowingContinuation { continuation in
// CHECK-NEXT:     foo { str, error in
// CHECK-NEXT:       if let error {
// CHECK-NEXT:         continuation.resume(throwing: error)
// CHECK-NEXT:       } else {
// CHECK-NEXT:         continuation.resume(returning: str)
// CHECK-NEXT:     }
