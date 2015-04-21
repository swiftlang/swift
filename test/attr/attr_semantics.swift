// RUN: %target-parse-verify-swift

@_semantics("foo") // expected-note {{attribute already specified here}}
@_semantics("bar") // expected-error {{duplicate attribute}}
func duplicatesemantics() {}

// Test parser recovery by having something that
// should parse fine.
func somethingThatShouldParseFine() {}


func func_with_nested_semantics() {
   @_semantics("exit") // expected-error {{attribute '_semantics' can only be used in a non-local scope}}
   func exit(code : UInt32) -> Void
   exit(0)
}

