// RUN: %target-typecheck-verify-swift -parse-as-library -target %target-swift-5.1-abi-triple -disable-implicit-concurrency-module-import
// RUN: %target-typecheck-verify-swift -parse-as-library -target %target-swift-5.1-abi-triple -parse-stdlib

@main struct Main {
  // expected-error@+1:22{{'_Concurrency' module not imported, required for async main}}
  static func main() async {
  }
}
