// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main
enum EntryPoint {
  static func main() {
  }
}
