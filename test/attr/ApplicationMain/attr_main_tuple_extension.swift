// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main
extension (Int, String) { // expected-error {{tuple type '(Int, String)' cannot be extended}}
  static func main() {
  }
}




