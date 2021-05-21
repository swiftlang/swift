// RUN: %empty-directory(%t)

enum CustomError : Error {
  case e
}

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 -enable-experimental-concurrency | %FileCheck -check-prefix=FOO1 %s
func foo1(_ completion: @escaping () -> Void) {}
// FOO1:      convert_async_wrapper.swift [[# @LINE-1]]:1 -> [[# @LINE-1]]:1
// FOO1-NEXT: @completionHandlerAsync("foo1()", completionHandlerIndex: 0)
// FOO1-EMPTY:
// FOO1-NEXT: convert_async_wrapper.swift [[# @LINE-4]]:49 -> [[# @LINE-4]]:49
// FOO1-EMPTY:
// FOO1-EMPTY:
// FOO1-EMPTY:
// FOO1-NEXT: convert_async_wrapper.swift [[# @LINE-8]]:49 -> [[# @LINE-8]]:49
// FOO1-NEXT: func foo1() async {
// FOO1-NEXT:   return await withCheckedContinuation { cont in
// FOO1-NEXT:     foo1() {
// FOO1-NEXT:       cont.resume(returning: ())
// FOO1-NEXT:     }
// FOO1-NEXT:   }
// FOO1-NEXT: }

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 -enable-experimental-concurrency | %FileCheck -check-prefix=FOO2 %s
func foo2(arg: String, _ completion: @escaping (String) -> Void) {}
// FOO2:      convert_async_wrapper.swift [[# @LINE-1]]:1 -> [[# @LINE-1]]:1
// FOO2-NEXT: @completionHandlerAsync("foo2(arg:)", completionHandlerIndex: 1)
// FOO2-EMPTY:
// FOO2-NEXT: convert_async_wrapper.swift [[# @LINE-4]]:68 -> [[# @LINE-4]]:68
// FOO2-EMPTY:
// FOO2-EMPTY:
// FOO2-EMPTY:
// FOO2-NEXT: convert_async_wrapper.swift [[# @LINE-8]]:68 -> [[# @LINE-8]]:68
// FOO2:      func foo2(arg: String) async -> String {
// FOO2-NEXT:   return await withCheckedContinuation { cont in
// FOO2-NEXT:     foo2(arg: arg) { res in
// FOO2-NEXT:       cont.resume(returning: res)
// FOO2-NEXT:     }
// FOO2-NEXT:   }
// FOO2-NEXT: }

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FOO3 %s
func foo3(arg: String, _ arg2: Int, _ completion: @escaping (String?) -> Void) {}

// FOO3:      func foo3(arg: String, _ arg2: Int) async -> String? {
// FOO3-NEXT:   return await withCheckedContinuation { cont in
// FOO3-NEXT:     foo3(arg: arg, arg2) { res in
// FOO3-NEXT:       cont.resume(returning: res)
// FOO3-NEXT:     }
// FOO3-NEXT:   }
// FOO3-NEXT: }

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FOO4 %s
func foo4(_ completion: @escaping (Error?) -> Void) {}

// FOO4:      func foo4() async throws {
// FOO4-NEXT:   return try await withCheckedThrowingContinuation { cont in
// FOO4-NEXT:     foo4() { err in
// FOO4-NEXT:       if let err = err {
// FOO4-NEXT:         cont.resume(throwing: err)
// FOO4-NEXT:         return
// FOO4-NEXT:       }
// FOO4-NEXT:       cont.resume(returning: ())
// FOO4-NEXT:     }
// FOO4-NEXT:   }
// FOO4-NEXT: }


// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FOO5 %s
func foo5(_ completion: @escaping (Error) -> Void) {}

// FOO5:      func foo5() async -> Error {
// FOO5-NEXT:   return await withCheckedContinuation { cont in
// FOO5-NEXT:     foo5() { res in
// FOO5-NEXT:       cont.resume(returning: res)
// FOO5-NEXT:     }
// FOO5-NEXT:   }
// FOO5-NEXT: }

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FOO6 %s
func foo6(_ completion: @escaping (String?, Error?) -> Void) {}

// FOO6:      func foo6() async throws -> String {
// FOO6-NEXT:   return try await withCheckedThrowingContinuation { cont in
// FOO6-NEXT:     foo6() { res, err in
// FOO6-NEXT:       if let err = err {
// FOO6-NEXT:         cont.resume(throwing: err)
// FOO6-NEXT:         return
// FOO6-NEXT:       }
// FOO6-NEXT:       guard let res = res else {
// FOO6-NEXT:         fatalError("Expected non-nil success param 'res' for nil error")
// FOO6-NEXT:       }
// FOO6-NEXT:       cont.resume(returning: res)
// FOO6-NEXT:     }
// FOO6-NEXT:   }
// FOO6-NEXT: }

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FOO7 %s
func foo7(_ completion: @escaping (String?, Int, Error?) -> Void) {}

// FOO7:      func foo7() async throws -> (String, Int) {
// FOO7-NEXT:   return try await withCheckedThrowingContinuation { cont in
// FOO7-NEXT:     foo7() { res1, res2, err in
// FOO7-NEXT:       if let err = err {
// FOO7-NEXT:         cont.resume(throwing: err)
// FOO7-NEXT:         return
// FOO7-NEXT:       }
// FOO7-NEXT:       guard let res1 = res1 else {
// FOO7-NEXT:         fatalError("Expected non-nil success param 'res1' for nil error")
// FOO7-NEXT:       }
// FOO7-NEXT:       cont.resume(returning: (res1, res2))
// FOO7-NEXT:     }
// FOO7-NEXT:   }
// FOO7-NEXT: }

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FOO8 %s
func foo8(_ completion: @escaping (String?, Int?, Error?) -> Void) {}

// FOO8:      func foo8() async throws -> (String, Int) {
// FOO8-NEXT:   return try await withCheckedThrowingContinuation { cont in
// FOO8-NEXT:     foo8() { res1, res2, err in
// FOO8-NEXT:       if let err = err {
// FOO8-NEXT:         cont.resume(throwing: err)
// FOO8-NEXT:         return
// FOO8-NEXT:       }
// FOO8-NEXT:       guard let res1 = res1 else {
// FOO8-NEXT:         fatalError("Expected non-nil success param 'res1' for nil error")
// FOO8-NEXT:       }
// FOO8-NEXT:       guard let res2 = res2 else {
// FOO8-NEXT:         fatalError("Expected non-nil success param 'res2' for nil error")
// FOO8-NEXT:       }
// FOO8-NEXT:       cont.resume(returning: (res1, res2))
// FOO8-NEXT:     }
// FOO8-NEXT:   }
// FOO8-NEXT: }

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FOO9 %s
func foo9(_ completion: @escaping (Result<String, Error>) -> Void) {}

// FOO9:      func foo9() async throws -> String {
// FOO9-NEXT:   return try await withCheckedThrowingContinuation { cont in
// FOO9-NEXT:     foo9() { res in
// FOO9-NEXT:       cont.resume(with: res)
// FOO9-NEXT:     }
// FOO9-NEXT:   }
// FOO9-NEXT: }

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FOO10 %s
func foo10(arg: Int, _ completion: @escaping (Result<(String, Int), Error>) -> Void) {}

// FOO10:      func foo10(arg: Int) async throws -> (String, Int) {
// FOO10-NEXT:   return try await withCheckedThrowingContinuation { cont in
// FOO10-NEXT:     foo10(arg: arg) { res in
// FOO10-NEXT:       cont.resume(with: res)
// FOO10-NEXT:     }
// FOO10-NEXT:   }
// FOO10-NEXT: }

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FOO11 %s
func foo11(completion: @escaping (Result<String, Never>) -> Void) {}

// FOO11:      func foo11() async -> String {
// FOO11-NEXT:   return await withCheckedContinuation { cont in
// FOO11-NEXT:     foo11() { res in
// FOO11-NEXT:       cont.resume(with: res)
// FOO11-NEXT:     }
// FOO11-NEXT:   }
// FOO11-NEXT: }

// RUN: %refactor-check-compiles -add-async-wrapper -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=FOO12 %s
func foo12(completion: @escaping (Result<String, CustomError>) -> Void) {}

// FOO12:      func foo12() async throws -> String {
// FOO12-NEXT:   return try await withCheckedThrowingContinuation { cont in
// FOO12-NEXT:     foo12() { res in
// FOO12-NEXT:       cont.resume(with: res)
// FOO12-NEXT:     }
// FOO12-NEXT:   }
// FOO12-NEXT: }
