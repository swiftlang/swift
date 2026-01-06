// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_protocol))  -target %target-swift-5.1-abi-triple -enable-library-evolution %S/Inputs/resilient_protocol.swift -emit-module -emit-module-path %t/resilient_protocol.swiftmodule -module-name resilient_protocol
// RUN: %target-codesign %t/%target-library-name(resilient_protocol)

// RUN: %target-build-swift -parse-as-library  -target %target-swift-5.1-abi-triple %s -lresilient_protocol -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(resilient_protocol)

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import resilient_protocol

enum MyError : Error {
	case bad
}

struct IntAwaitable : Awaitable {
  func waitForNothing() async {}

  func wait() async -> Int {
    return 123
  }

  func waitForInt() async -> Int {
    return 321
  }

  func wait(orThrow: Bool) async throws {
    if (orThrow) {
      throw MyError.bad
    }
  }
}

func genericWaitForNothing<T : Awaitable>(_ t: T) async {
  await t.waitForNothing()
}

func genericWait<T : Awaitable>(_ t: T) async -> T.Result {
  return await t.wait()
}

func genericWaitForInt<T : Awaitable>(_ t: T) async -> Int {
  return await t.waitForInt()
}

func genericWait<T : Awaitable>(orThrow: Bool, _ t: T) async throws {
  return try await t.wait(orThrow: orThrow)
}

@main struct Main {
  static func main() async {
    let task = Task.detached {
      var AsyncProtocolRequirementSuite = TestSuite("ResilientProtocol")

      AsyncProtocolRequirementSuite.test("AsyncProtocolRequirement") {
        let x = IntAwaitable()

        await genericWaitForNothing(x)

        expectEqual(123, await genericWait(x))
        expectEqual(321, await genericWaitForInt(x))

        expectNil(try? await genericWait(orThrow: true, x))
        try! await genericWait(orThrow: false, x)
      }
      await runAllTestsAsync()
    }

    await task.value
  }
}
