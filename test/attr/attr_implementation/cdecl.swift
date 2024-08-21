// RUN: %target-typecheck-verify-swift -import-objc-header %S/Inputs/cdecl_implementation.h -enable-experimental-feature CImplementation -target %target-stable-abi-triple
// REQUIRES: objc_interop

//
// @_cdecl for global functions
//

@implementation @_cdecl("CImplFunc1")
func CImplFunc1(_: Int32) {
  // OK
}

@implementation(BadCategory) @_cdecl("CImplFunc2")
func CImplFunc2(_: Int32) {
  // expected-error@-2 {{global function 'CImplFunc2' does not belong to an Objective-C category; remove the category name from this attribute}} {{16-29=}}
}

@implementation @_cdecl("CImplFuncMissing")
func CImplFuncMissing(_: Int32) {
  // expected-error@-2 {{could not find imported function 'CImplFuncMissing' matching global function 'CImplFuncMissing'; make sure your umbrella or bridging header imports the header that declares it}}
}

@implementation @_cdecl("CImplFuncMismatch1")
func CImplFuncMismatch1(_: Float) {
  // expected-error@-1 {{global function 'CImplFuncMismatch1' of type '(Float) -> ()' does not match type '(Int32) -> Void' declared by the header}}
}

@implementation @_cdecl("CImplFuncMismatch2")
func CImplFuncMismatch2(_: Int32) -> Float {
  // expected-error@-1 {{global function 'CImplFuncMismatch2' of type '(Int32) -> Float' does not match type '(Int32) -> Void' declared by the header}}
}

@implementation @_cdecl("CImplFuncMismatch3")
func CImplFuncMismatch3(_: UnsafeMutableRawPointer?) {
  // OK
}

@implementation @_cdecl("CImplFuncMismatch4")
func CImplFuncMismatch4(_: UnsafeMutableRawPointer) {
  // expected-error@-1 {{global function 'CImplFuncMismatch4' of type '(UnsafeMutableRawPointer) -> ()' does not match type '(UnsafeMutableRawPointer?) -> Void' declared by the header}}
}

@implementation @_cdecl("CImplFuncMismatch5")
func CImplFuncMismatch5(_: UnsafeMutableRawPointer) {
  // OK
}

@implementation @_cdecl("CImplFuncMismatch6")
func CImplFuncMismatch6(_: UnsafeMutableRawPointer!) {
  // OK, mismatch allowed
}

@implementation @_cdecl("CImplFuncMismatch3a")
func CImplFuncMismatch3a(_: Int32) -> UnsafeMutableRawPointer? {
  // OK
}

@implementation @_cdecl("CImplFuncMismatch4a")
func CImplFuncMismatch4a(_: Int32) -> UnsafeMutableRawPointer {
  // expected-error@-1 {{global function 'CImplFuncMismatch4a' of type '(Int32) -> UnsafeMutableRawPointer' does not match type '(Int32) -> UnsafeMutableRawPointer?' declared by the header}}
}

@implementation @_cdecl("CImplFuncMismatch5a")
func CImplFuncMismatch5a(_: Int32) -> UnsafeMutableRawPointer {
  // OK
}

@implementation @_cdecl("CImplFuncMismatch6a")
func CImplFuncMismatch6a(_: Int32) -> UnsafeMutableRawPointer! {
  // OK, mismatch allowed
}

@implementation @_cdecl("CImplFuncNameMismatch1")
func mismatchedName1(_: Int32) {
  // expected-error@-2 {{could not find imported function 'CImplFuncNameMismatch1' matching global function 'mismatchedName1'; make sure your umbrella or bridging header imports the header that declares it}}
  // FIXME: Improve diagnostic for a partial match.
}

@implementation @_cdecl("mismatchedName2")
func CImplFuncNameMismatch2(_: Int32) {
  // expected-error@-2 {{could not find imported function 'mismatchedName2' matching global function 'CImplFuncNameMismatch2'; make sure your umbrella or bridging header imports the header that declares it}}
  // FIXME: Improve diagnostic for a partial match.
}

//
// TODO: @_cdecl for global functions imported as computed vars
//
var cImplComputedGlobal1: Int32 {
  @implementation @_cdecl("CImplGetComputedGlobal1")
  get {
    // FIXME: Lookup for vars isn't working yet
    // expected-error@-3 {{could not find imported function 'CImplGetComputedGlobal1' matching getter for var 'cImplComputedGlobal1'; make sure your umbrella or bridging header imports the header that declares it}}
    return 0
  }

  @implementation @_cdecl("CImplSetComputedGlobal1")
  set {
    // FIXME: Lookup for vars isn't working yet
    // expected-error@-3 {{could not find imported function 'CImplSetComputedGlobal1' matching setter for var 'cImplComputedGlobal1'; make sure your umbrella or bridging header imports the header that declares it}}
    print(newValue)
  }
}

//
// TODO: @_cdecl for import-as-member functions
//
extension CImplStruct {
  @implementation @_cdecl("CImplStructStaticFunc1")
  static func staticFunc1(_: Int32) {
    // FIXME: Add underlying support for this
    // expected-error@-3 {{@_cdecl can only be applied to global functions}}
    // FIXME: Lookup in an enclosing type is not working yet
    // expected-error@-5 {{could not find imported function 'CImplStructStaticFunc1' matching static method 'staticFunc1'; make sure your umbrella or bridging header imports the header that declares it}}
  }
}

func usesAreNotAmbiguous() {
  CImplFunc1(42)
}
