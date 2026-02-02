// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)  | %FileCheck %s
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: reflection

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

struct Boom: Error {}

func boom() throws -> Int {
  throw Boom()
}

@available(SwiftStdlib 5.1, *)
func test() async {
  async let result = boom()

  do {
    _ = try await result
  } catch {
    print("error: \(error)") // CHECK: error: Boom()
  }
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test()
  }
}
