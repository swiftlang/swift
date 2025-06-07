// RUN: %empty-directory(%t)
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple %s -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

public dynamic func number() async -> Int {
    return 100
}

enum SomeError : Error {
  case err
}

class C {
  dynamic func a() async throws -> Int? {
    return 0
  }

  dynamic func b() async throws {
    guard let data = try await a() else {
      throw SomeError.err
    }
  }

}
@main struct Main {

  static func main() async {
    do {
      try await C().b()
    } catch _ { assertionFailure("should not throw") }

    // CHECK: 100
    let value = await number()
    print(value)
  }
}
