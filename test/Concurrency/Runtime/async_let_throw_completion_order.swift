// rdar://81481317
// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)  | %FileCheck %s
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault
// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://82123254
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

struct Bad: Error {}

class Foo { init() async throws {}; deinit { print("Foo down") } }
class Bar { init() async throws { throw Bad() }; deinit { print("Bar down") } }
class Baz { init() async throws {}; deinit { print("Baz down") } }

func zim(y: Bar, x: Foo, z: Baz) { print("hooray") }

@main struct Butt {

  static func main() async {
    do {
      async let x = Foo()
      async let y = Bar()
      async let z = Baz()

      return try await zim(y: y, x: x, z: z)
    } catch {
      // CHECK: oopsie whoopsie
      print("oopsie whoopsie")
    }
  }
}
