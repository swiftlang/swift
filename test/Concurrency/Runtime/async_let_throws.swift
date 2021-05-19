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

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test() async {
  async let result = boom()

  do {
    _ = try await result
  } catch {
    print("error: \(error)") // CHECK: error: Boom()
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await test()
  }
}
