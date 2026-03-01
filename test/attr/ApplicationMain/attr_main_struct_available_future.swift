// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

// REQUIRES: OS=macosx

@main // expected-error {{'main()' is only available in macOS 99 or newer}}
@available(OSX 10.0, *)
struct EntryPoint {
// expected-note@-1 {{update '@available' attribute on enclosing}}
  @available(OSX 99, *)
  static func main() {
  }
}


