// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

enum EntryPoint {
}

@main
extension EntryPoint {
  static func main() {
  }
}



