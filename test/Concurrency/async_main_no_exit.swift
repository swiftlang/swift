// RUN: %target-typecheck-verify-swift -parse-as-library -disable-availability-checking -parse-stdlib
// expect-no-diagnostics

import _Concurrency

@main struct Main {
  static func main() async {
  }
}
