// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main // expected-error{{'main' attribute can only apply to one type in a module}}
struct MyMain1 {
  static func main() {
  }
}

@main // expected-error{{'main' attribute can only apply to one type in a module}}
enum MyMain2 {
  static func main() {
  }
}

@main // expected-error{{'main' attribute can only apply to one type in a module}}
class MyMain3 {
  static func main() {
  }
}
