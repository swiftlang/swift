// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency %s -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

public dynamic func number() async -> Int {
    return 100
}

@main struct Main {
  static func main() async {
    // CHECK: 100
    let value = await number()
    print(value)
  }
}
