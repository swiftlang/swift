// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

enum SomeError: Error {
  case bad
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    let condition = false

    let t1 = Task {
      return 5
    }

    let t2 = Task { () -> Int in
      if condition {
        throw SomeError.bad
      }

      return 7
    }

    let t2f = Task { () throws(SomeError) -> Int in
      throw SomeError.bad
    }
    do {
      _ = try await t2f.value
    } catch {
      let err: SomeError = error // confirm it was a typed throw
      _ = err
    }

    let t3 = Task.detached {
      return 9
    }

    let t4 = Task.detached { () -> Int in
      if condition {
        throw SomeError.bad
      }

      return 11
    }

    let t4f = Task.detached { () throws(SomeError) -> Int in
      if condition {
        throw SomeError.bad
      }

      return 11
    }
    do {
      _ = try await t4f.value
    } catch {
      let err: SomeError = error // confirm it was a typed throw
      _ = err
    }

    if #available(SwiftStdlib 6.2, *) {
      let t5f = Task.immediate { () throws(SomeError) -> Int in
        if condition {
          throw SomeError.bad
        }

        return 11
      }
      do {
        _ = try await t5f.value
      } catch {
        let err: SomeError = error // confirm it was a typed throw
        _ = err
      }

      let t6f = Task.immediateDetached { () throws(SomeError) -> Int in
        if condition {
          throw SomeError.bad
        }

        return 11
      }
      do {
        _ = try await t6f.value
      } catch {
        let err: SomeError = error // confirm it was a typed throw
        _ = err
      }
    }

    let result = try! await t1.value + t2.value + t3.value + t4.value
    assert(result == 32)
  }
}
