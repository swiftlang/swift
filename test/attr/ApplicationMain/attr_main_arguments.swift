// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main // expected-error{{'MyBase' is annotated with @main and must provide a main static function}}
struct MyBase {
  static func main(_ argc: Int, _ argv: [String]) {
  }
}

