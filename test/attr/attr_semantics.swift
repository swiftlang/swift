// RUN: %target-typecheck-verify-swift

@_semantics("foo")
@_semantics("bar")
func duplicatesemantics() {}

func func_with_nested_semantics_1() {
   @_semantics("exit") // expected-error {{attribute '_semantics' can only be used in a non-local scope}}
   func exit(_ code : UInt32) -> Void
   exit(0)
}

// Test parser recovery by having something that
// should parse fine.
func somethingThatShouldParseFine() {}

func func_with_nested_semantics_2() {
   @_semantics("exit") // expected-error {{attribute '_semantics' can only be used in a non-local scope}}
   func exit(_ code : UInt32) -> Void
   exit(0)
}
