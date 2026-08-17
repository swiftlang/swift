// RUN: %target-typecheck-verify-swift -target %target-stable-abi-triple \
// RUN:   -import-bridging-header %S/Inputs/objc_implementation.h \
// RUN:   -I %S/Inputs -verify-ignore-unrelated

// REQUIRES: objc_interop

// @objc @implementation on a top-level function (SE-0495) implements a
// function declared in an imported Objective-C header, just like @implementation
// @c, but accepting Objective-C types.

@objc @implementation
func CImplFunc1(_: Int32) {
  // OK
}

@objc(CImplFuncRenamed_C) @implementation
func CImplFuncRenamed_Swift(arg: CInt) {
  // OK: the @objc(name) argument selects the imported C symbol.
}

@objc @implementation
func CImplFuncMissing(_: Int32) {
  // expected-error@-2 {{could not find imported function 'CImplFuncMissing' matching global function 'CImplFuncMissing'; make sure you import the module or header that declares it}}
}

@objc @implementation
func CImplFuncMismatch1(_: Float) {
  // expected-error@-1 {{global function 'CImplFuncMismatch1' of type '(Float) -> ()' does not match type '(Int32) -> Void' declared by the header}}
}

@objc @implementation
func CImplFuncMismatch2(_: Int32) -> Float {
  // expected-error@-1 {{global function 'CImplFuncMismatch2' of type '(Int32) -> Float' does not match type '(Int32) -> Void' declared by the header}}
}
