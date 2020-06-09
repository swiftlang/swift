// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main
struct MyBase {
  static func main() throws { 
  }
}

