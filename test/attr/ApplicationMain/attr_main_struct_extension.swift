// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

struct EntryPoint {
}

@main
extension EntryPoint {
  static func main() {
  }
}

