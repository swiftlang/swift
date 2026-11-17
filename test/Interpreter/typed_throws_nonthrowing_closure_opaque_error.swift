// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-5.1-abi-triple) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// Non-throwing closure literals passed to parameters whose thrown error is a
// generic parameter of the callee are emitted directly at the destination
// type, with an indirect error result that the closure never writes. Make
// sure calling them through that indirect-error convention works, and that
// throwing closures in the same positions still propagate their errors.

struct E: Error, Equatable {
  var value: Int
}

struct Runner<Failure: Error> {
  let fn: () throws(Failure) -> Int
  init(_ fn: @escaping () throws(Failure) -> Int) { self.fn = fn }
  func run() throws(Failure) -> Int { try fn() }
}

struct AsyncRunner<Failure: Error>: Sendable {
  let fn: @isolated(any) () async throws(Failure) -> Int
  init(_ fn: sending @escaping @isolated(any) () async throws(Failure) -> Int) {
    self.fn = fn
  }
  func run() async throws(Failure) -> Int { try await fn() }
}

func runGeneric<Failure: Error>(_ runner: Runner<Failure>) -> String {
  do {
    return "value \(try runner.run())"
  } catch {
    return "error \(error)"
  }
}

@MainActor
func mainActorValue() -> Int { 42 }

@main struct Main {
  static func main() async {
    // CHECK: sync any Error: value 1
    let r1: Runner<any Error> = Runner { 1 }
    print("sync any Error:", runGeneric(r1))

    // CHECK: sync E: value 2
    let r2: Runner<E> = Runner { 2 }
    print("sync E:", runGeneric(r2))

    // CHECK: sync throwing: error E(value: 3)
    let r3: Runner<any Error> = Runner { throw E(value: 3) }
    print("sync throwing:", runGeneric(r3))

    // CHECK: async any Error: 4
    let a1: AsyncRunner<any Error> = AsyncRunner { 4 }
    print("async any Error:", (try? await a1.run()) ?? -1)

    // CHECK: async main actor: 42
    let a2: AsyncRunner<any Error> = AsyncRunner { @MainActor in mainActorValue() }
    print("async main actor:", (try? await a2.run()) ?? -1)

    // CHECK: async throwing: E(value: 5)
    let a3: AsyncRunner<any Error> = AsyncRunner { throw E(value: 5) }
    do {
      _ = try await a3.run()
      print("async throwing: no error")
    } catch {
      print("async throwing:", error)
    }

    // CHECK: async typed: E(value: 6)
    let a4: AsyncRunner<E> = AsyncRunner { () throws(E) in throw E(value: 6) }
    do {
      _ = try await a4.run()
      print("async typed: no error")
    } catch {
      print("async typed:", error)
    }
  }
}
