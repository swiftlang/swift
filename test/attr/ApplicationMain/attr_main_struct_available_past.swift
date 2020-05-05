// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

// REQUIRES: OS=macosx

@main
struct EntryPoint {
  @available(OSX 10.0, *)
  static func main() {
  }
}

