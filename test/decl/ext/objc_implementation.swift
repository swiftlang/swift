// RUN: %target-typecheck-verify-swift -import-objc-header %S/Inputs/objc_implementation.h
// REQUIRES: objc_interop

// FIXME: Should complain about method(fromHeader4:)
@_objcImplementation extension ObjCClass {
  func method(fromHeader1: CInt) {
    // FIXME: OK, provides an implementation for the header's method.
  }

  @objc func method(fromHeader2: CInt) {
    // OK, provides an implementation for the header's method.
  }

  func categoryMethod(fromHeader3: CInt) {
    // FIXME: Should complain about the wrong category
  }

  @objc fileprivate func methodNot(fromHeader1: CInt) {
    // OK, declares a new @objc dynamic method.
  }

  final func methodNot(fromHeader2: CInt) {
    // OK, declares a new Swift method.
  }

  func methodNot(fromHeader3: CInt) {
    // FIXME: Should complain about unmatched, un-attributed method
  }
}

// FIXME: Should complain about categoryMethodFromHeader4:
@_objcImplementation(PresentAdditions) extension ObjCClass {
  func method(fromHeader3: CInt) {
    // FIXME: Should complain about wrong category
  }

  func categoryMethod(fromHeader1: CInt) {
    // FIXME: OK, provides an implementation for the header's method.
    // OK, provides an implementation for the header's method.
  }

  @objc fileprivate func categoryMethodNot(fromHeader1: CInt) {
    // OK, declares a new @objc dynamic method.
  }

  final func categoryMethodNot(fromHeader2: CInt) {
    // OK, declares a new Swift method.
  }

  func categoryMethodNot(fromHeader3: CInt) {
    // FIXME: Should complain about unmatched, un-attributed method
  }
}

@_objcImplementation(PresentAdditions) extension ObjCClass {} // FIXME: should complain about duplicate

@_objcImplementation(MissingAdditions) extension ObjCClass {}
// expected-error@-1 {{could not find category 'MissingAdditions' on Objective-C class 'ObjCClass'; make sure your umbrella or bridging header imports the header that declares it}}
// expected-note@-2 {{remove arguments to implement the main '@interface' for this class}} {{21-39=}}

@_objcImplementation extension ObjCStruct {}
// expected-error@-1 {{cannot mark extension of struct 'ObjCStruct' with '@_objcImplementation'; it is not an imported Objective-C class}} {{1-22=}}

@_objcImplementation(CantWork) extension ObjCStruct {}
// expected-error@-1 {{cannot mark extension of struct 'ObjCStruct' with '@_objcImplementation'; it is not an imported Objective-C class}} {{1-32=}}

@objc class SwiftClass {}
// expected-note@-1 2 {{'SwiftClass' declared here}}

@_objcImplementation extension SwiftClass {}
// expected-error@-1 {{'@_objcImplementation' cannot be used to extend class 'SwiftClass' because it was defined by a Swift 'class' declaration, not an imported Objective-C '@interface' declaration}} {{1-22=}}

@_objcImplementation(WTF) extension SwiftClass {} // expected
// expected-error@-1 {{'@_objcImplementation' cannot be used to extend class 'SwiftClass' because it was defined by a Swift 'class' declaration, not an imported Objective-C '@interface' declaration}} {{1-27=}}
