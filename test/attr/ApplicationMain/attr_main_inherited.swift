// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

class MainBase {
  static func main() {
  }
}

@main
class Main : MainBase { }

