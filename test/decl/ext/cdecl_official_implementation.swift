// RUN: %target-typecheck-verify-swift -target %target-stable-abi-triple \
// RUN:   -import-bridging-header %S/Inputs/objc_implementation.h \
// RUN:   -I %S/Inputs \
// RUN:   -disable-objc-interop \
// RUN:   -enable-experimental-feature CImplementation \
// RUN:   -enable-experimental-feature CDecl

// REQUIRES: swift_feature_CImplementation
// REQUIRES: swift_feature_CDecl

import ToBeImplemented

@implementation @c
func c_function_returns_double() -> Double { 0 }

@implementation @c
func CImplFunc1(_: Int32) {
  // OK
}

@implementation @c(CImplFuncRenamed_C)
func CImplFuncRenamed_Swift(arg: CInt) {
  // OK
}

@implementation(BadCategory) @c
func CImplFunc2(_: Int32) {
  // expected-error@-2 {{global function 'CImplFunc2' does not belong to an Objective-C category; remove the category name from this attribute}} {{16-29=}}
}

@implementation @c
func CImplFuncMissing(_: Int32) {
  // expected-error@-2 {{could not find imported function 'CImplFuncMissing' matching global function 'CImplFuncMissing'; make sure you import the module or header that declares it}}
}

@implementation @c
func CImplFuncMismatch1(_: Float) {
  // expected-error@-1 {{global function 'CImplFuncMismatch1' of type '(Float) -> ()' does not match type '(Int32) -> Void' declared by the header}}
}

@implementation @c
func CImplFuncMismatch2(_: Int32) -> Float {
  // expected-error@-1 {{global function 'CImplFuncMismatch2' of type '(Int32) -> Float' does not match type '(Int32) -> Void' declared by the header}}
}

@implementation @c(CImplFuncNameMismatch1)
func mismatchedName1(_: Int32) {
  // expected-error@-2 {{could not find imported function 'CImplFuncNameMismatch1' matching global function 'mismatchedName1'; make sure you import the module or header that declares it}}
  // FIXME: Improve diagnostic for a partial match.
}

@implementation @c(mismatchedName2)
func CImplFuncNameMismatch2(_: Int32) {
  // expected-error@-2 {{could not find imported function 'mismatchedName2' matching global function 'CImplFuncNameMismatch2'; make sure you import the module or header that declares it}}
  // FIXME: Improve diagnostic for a partial match.
}

//
// TODO: @c for global functions imported as computed vars
//
var cImplComputedGlobal1: Int32 {
  @implementation @c(CImplGetComputedGlobal1)
  get {
    // FIXME: Lookup for vars isn't working yet
    // expected-error@-3 {{could not find imported function 'CImplGetComputedGlobal1' matching getter for var 'cImplComputedGlobal1'; make sure you import the module or header that declares it}}
    return 0
  }

  @implementation @c(CImplSetComputedGlobal1)
  set {
    // FIXME: Lookup for vars isn't working yet
    // expected-error@-3 {{could not find imported function 'CImplSetComputedGlobal1' matching setter for var 'cImplComputedGlobal1'; make sure you import the module or header that declares it}}
    print(newValue)
  }
}

//
// TODO: @c for import-as-member functions
//
extension CImplStruct {
  @implementation @c(CImplStructStaticFunc1)
  static func staticFunc1(_: Int32) {
    // FIXME: Add underlying support for this
    // expected-error@-3 {{@c can only be applied to global functions}}
    // FIXME: Lookup in an enclosing type is not working yet
    // expected-error@-5 {{could not find imported function 'CImplStructStaticFunc1' matching static method 'staticFunc1'; make sure you import the module or header that declares it}}
  }
}
