// RUN: %target-parse-verify-swift

@asmname("foo") // expected-note {{attribute already specified here}}
@asmname("bar") // expected-error {{duplicate attribute}}
func duplicateAsmName() {}

// Test parser recovery by having something that
// should parse fine.
func somethingThatShouldParseFine() {}

func func_with_nested_asmname() {
   @asmname("exit") // expected-error {{attribute 'asmname' can only be used in a non-local scope}}
   func exit(code : UInt32) -> Void
   exit(0)
}

