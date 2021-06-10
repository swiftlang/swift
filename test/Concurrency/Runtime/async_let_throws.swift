// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

struct Boom: Error {}

func boom() throws -> Int {
  throw Boom()
}

@available(SwiftStdlib 5.5, *)
func test() async {
  async let result = boom()

  do {
    _ = try await result
  } catch {
    print("error: \(error)") // CHECK: error: Boom()
  }
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test()
  }
}
