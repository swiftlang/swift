// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

struct EntryPoint {
  static func main() {
  }
}

@main
extension EntryPoint {
}

