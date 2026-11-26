// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple \
// RUN:   -parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple \
// RUN:   -parse-as-library -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: reflection
// UNSUPPORTED: back_deployment_runtime

// https://github.com/swiftlang/swift/issues/92312
//
// A closure literal with a typed throws clause passed to a generic parameter
// with an untyped `throws` (e.g. `Task.init`) must deliver the thrown error
// intact through the type-erased error result.

enum EnumError: Error, Equatable { case a, b, c }

struct RequestError: Error, Equatable {
  let code: Int
  let message: String
}

struct PayloadError: Error, Equatable {
  var flag: Bool
  var count: Int?
}

final class ClassError: Error {
  let id: Int
  init(id: Int) { self.id = id }
}

func check<E: Error & Equatable>(_ label: String, _ error: any Error,
                                 _ expected: E) {
  print(label, error, (error as? E) == expected)
}

func checkClass(_ label: String, _ error: any Error, _ expected: ClassError) {
  print(label, (error as? ClassError)?.id ?? -1,
        (error as? ClassError) === expected)
}

func callSync<T>(_ op: () throws -> T) -> (any Error)? {
  do {
    _ = try op()
    return nil
  } catch {
    return error
  }
}

func callAsync<T>(_ op: () async throws -> T) async -> (any Error)? {
  do {
    _ = try await op()
    return nil
  } catch {
    return error
  }
}

func callNonsending<T>(
  _ op: nonisolated(nonsending) () async throws -> T
) async -> (any Error)? {
  do {
    _ = try await op()
    return nil
  } catch {
    return error
  }
}

@MainActor func callMainActor<T>(
  _ op: @MainActor () async throws -> T
) async -> (any Error)? {
  do {
    _ = try await op()
    return nil
  } catch {
    return error
  }
}

func throwRequest() async throws(RequestError) -> Int {
  throw RequestError(code: 42, message: "hello")
}

@main struct Main {
  static func main() async {
    // Task.result

    let t1 = Task { @Sendable () async throws(EnumError) -> Int in throw .c }
    if case .failure(let e) = await t1.result {
      check("task enum:", e, EnumError.c)
    }
    // CHECK: task enum: c true

    let t2 = Task { () async throws(RequestError) -> Int in
      try await throwRequest()
    }
    if case .failure(let e) = await t2.result {
      check("task struct:", e, RequestError(code: 42, message: "hello"))
    }
    // CHECK: task struct: RequestError(code: 42, message: "hello") true

    let t3 = Task { () async throws(PayloadError) -> Int in
      throw PayloadError(flag: true, count: nil)
    }
    if case .failure(let e) = await t3.result {
      check("task payload:", e, PayloadError(flag: true, count: nil))
    }
    // CHECK: task payload: PayloadError(flag: true, count: nil) true

    let classError = ClassError(id: 7)
    let t4 = Task { () async throws(ClassError) -> Int in throw classError }
    if case .failure(let e) = await t4.result {
      checkClass("task class:", e, classError)
    }
    // CHECK: task class: 7 true

    let t5 = Task { () async throws(EnumError) -> Int in throw .b }
    do {
      _ = try await t5.value
    } catch {
      check("task value enum:", error, EnumError.b)
    }
    // CHECK: task value enum: b true

    // Generic parameter with an untyped `throws`, without Task.

    if let e = callSync({ () throws(EnumError) -> Int in throw .c }) {
      check("sync enum:", e, EnumError.c)
    }
    // CHECK: sync enum: c true

    if let e = callSync({ () throws(RequestError) -> Int in
      throw RequestError(code: 1, message: "sync")
    }) {
      check("sync struct:", e, RequestError(code: 1, message: "sync"))
    }
    // CHECK: sync struct: RequestError(code: 1, message: "sync") true

    if let e = callSync({ () throws(ClassError) -> Int in throw classError }) {
      checkClass("sync class:", e, classError)
    }
    // CHECK: sync class: 7 true

    if let e = await callAsync({ () async throws(EnumError) -> Int in
      throw .a
    }) {
      check("async enum:", e, EnumError.a)
    }
    // CHECK: async enum: a true

    if let e = await callAsync({ () async throws(RequestError) -> Int in
      try await throwRequest()
    }) {
      check("async struct:", e, RequestError(code: 42, message: "hello"))
    }
    // CHECK: async struct: RequestError(code: 42, message: "hello") true

    if let e = await callAsync({ () async throws(PayloadError) -> Int in
      throw PayloadError(flag: false, count: 3)
    }) {
      check("async payload:", e, PayloadError(flag: false, count: 3))
    }
    // CHECK: async payload: PayloadError(flag: false, count: Optional(3)) true

    if let e = await callAsync({ () async throws(ClassError) -> Int in
      throw classError
    }) {
      checkClass("async class:", e, classError)
    }
    // CHECK: async class: 7 true

    if let e = await callNonsending({ () async throws(RequestError) -> Int in
      try await throwRequest()
    }) {
      check("nonsending struct:", e, RequestError(code: 42, message: "hello"))
    }
    // CHECK: nonsending struct: RequestError(code: 42, message: "hello") true

    if let e = await callMainActor({ () async throws(PayloadError) -> Int in
      throw PayloadError(flag: true, count: 9)
    }) {
      check("main actor payload:", e, PayloadError(flag: true, count: 9))
    }
    // CHECK: main actor payload: PayloadError(flag: true, count: Optional(9)) true

    // The closure does not throw: the normal result must be unaffected.
    let t6 = Task { () async throws(RequestError) -> Int in 5 }
    print("task success:", (try? await t6.value) ?? -1)
    // CHECK: task success: 5

    print("done")
    // CHECK: done
  }
}
