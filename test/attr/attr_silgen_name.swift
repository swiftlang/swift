// RUN: %target-typecheck-verify-swift -enable-experimental-feature Extern

@_silgen_name("foo") // expected-note {{attribute already specified here}}
@_silgen_name("bar") // expected-error {{duplicate attribute}}
func duplicateAsmName() {}

// Test parser recovery by having something that
// should parse fine.
func somethingThatShouldParseFine() {}

func func_with_nested__silgen_name() {
   @_silgen_name("exit") // expected-error {{attribute '_silgen_name' can only be used in a non-local scope}}
   func exit(_ code : UInt32) -> Void
   exit(0)
}

// Ensure that magic runtime symbol names can't be declared or defined through
// various symbol-assigning attributes

@_silgen_name("swift_retain") // expected-warning{{reserved}}
func liveDangerously() {}

@_silgen_name("swift_retain") // expected-warning{{reserved}}
func liveRecklessly();

@_extern(c, "swift_retain") // expected-warning{{reserved}}
func liveEphemerally()

@_cdecl("swift_retain") // expected-warning{{reserved}}
func liveFrivolously() {}
