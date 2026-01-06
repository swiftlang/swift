// RUN: %target-typecheck-verify-swift -target %target-pre-stable-abi-triple

// REQUIRES: concurrency
// REQUIRES: swift_swift_parser
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// rdar://126118470
// UNSUPPORTED: CPU=arm64e

@available(SwiftStdlib 5.1, *)
func isolatedFunc(isolation: isolated (any Actor)? = #isolation) {}

func test() { // expected-note 3 {{add '@available' attribute to enclosing global function}}
  _ = #isolation // expected-error {{'isolation()' is only available in}} expected-note {{add 'if #available' version check}}
  isolatedFunc() // expected-error {{'isolatedFunc(isolation:)' is only available in}} expected-note {{add 'if #available' version check}}
  // expected-error@-1 {{'isolation()' is only available in}}

  if #available(SwiftStdlib 5.1, *) {
    _ = #isolation
    isolatedFunc()
  }
}

@available(SwiftStdlib 5.1, *)
func testAvailable5_1() {
  _ = #isolation
  isolatedFunc()
}
