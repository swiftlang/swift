// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main
extension Int.Type { // expected-error {{cannot extend a metatype 'Int.Type'}}
  static func main() {
  }
}




