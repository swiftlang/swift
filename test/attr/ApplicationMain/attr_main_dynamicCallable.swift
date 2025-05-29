// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main // expected-error{{'Foo' is annotated with '@main' and must provide a main static function}}
struct Foo {
  @dynamicCallable
  struct main {
    func dynamicallyCall(withArguments args: [Any]) -> () { return }
  }
}
