// RUN: %target-typecheck-verify-swift -parse-as-library -disable-availability-checking -disable-implicit-concurrency-module-import %s
// RUN: %target-typecheck-verify-swift -parse-as-library  -disable-availability-checking -parse-stdlib %s

@main struct Main {
  // expected-error@+1:22{{'_Concurrency' module not imported, required for async main}}
  static func main() async {
  }
}
