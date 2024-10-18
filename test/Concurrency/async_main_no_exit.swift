// RUN: %target-typecheck-verify-swift -parse-as-library -target %target-swift-5.1-abi-triple -parse-stdlib
// expect-no-diagnostics

import _Concurrency

@main struct Main {
  static func main() async {
  }
}
