// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

class EntryPoint {
}

@main
extension EntryPoint {
  static func main() {
  }
}


