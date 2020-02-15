// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main // expected-error{{'MyBase' is annotated with @main and must provide a main static function of type () -> ().}}
struct MyBase {
  static func main() throws { 
  }
}

