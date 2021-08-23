// RUN: %empty-directory(%t)
// RUN: %target-build-swift  -Xfrontend -disable-availability-checking %s -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

public dynamic func number() async -> Int {
    return 100
}

@_dynamicReplacement(for: number())
internal func _replacement_number() async -> Int {
    return 200
}

@main struct Main {
  static func main() async {
      // CHECK: 200
    let value = await number()
    print(value)
  }
}
