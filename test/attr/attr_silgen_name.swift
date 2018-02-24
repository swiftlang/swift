// RUN: %target-typecheck-verify-swift -parse-stdlib

import Swift

@_silgen_name("foo") // expected-note {{attribute already specified here}}
@_silgen_name("bar") // expected-error {{duplicate attribute}}
func duplicateAsmName() {}

// Test parser recovery by having something that
// should parse fine.
func somethingThatShouldParseFine() {}

// Allow _silgen_name accessors without bodies.
var _silgen_nameGet1: Int {
  @_silgen_name("get1") get
  set { }
}

var _silgen_nameGet2: Int {
  set { }
  @_silgen_name("get2") get
}

var _silgen_nameGet3: Int {
  @_silgen_name("get3") get
}

var _silgen_nameGetSet: Int {
  @_silgen_name("get4") get
  @_silgen_name("set4") set
}

func func_with_nested__silgen_name() {
   @_silgen_name("exit") // expected-error {{attribute '_silgen_name' can only be used in a non-local scope}}
   func exit(_ code : UInt32) -> Void
   exit(0)
}
