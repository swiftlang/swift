// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main // expected-error{{generic 'main' types are not supported}}
class MyBase<T> {
  static func main() {
  }
}
