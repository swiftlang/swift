// RUN: %target-typecheck-verify-swift -import-objc-header %S/Inputs/objc_implementation.h
// RUN: %target-typecheck-verify-swift -DPRIVATE_MODULE -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_private.modulemap

// REQUIRES: objc_interop

// Swift doesn't diagnose selector conflicts if there are other errors
// in the file. This is equivalent to decl/ext/objc_implementation.swift
// but has no failures, so we get to that stage of type checking.

#if PRIVATE_MODULE
import objc_implementation_private
#endif

// FIXME: Should complain about method(fromHeader4:) and propertyFromHeader9
@_objcImplementation extension ObjCClass {
  @objc func method(fromHeader1: CInt) {
    // OK, provides an implementation for the header's method.
  }

  @objc func method(fromHeader2: CInt) {
    // OK, provides an implementation for the header's method.
  }

  @objc func method(fromHeader3: CInt) {
    // OK
  }

  @objc fileprivate func methodNot(fromHeader1: CInt) {
    // OK, declares a new @objc dynamic method.
  }

  final func methodNot(fromHeader2: CInt) {
    // OK, declares a new Swift method.
  }

  @objc var propertyFromHeader1: CInt
  // OK, provides an implementation with a stored property

  @objc var propertyFromHeader2: CInt
  // OK, provides an implementation with a stored property

  @objc var propertyFromHeader3: CInt {
    // OK, provides an implementation with a computed property
    get { return 1 }
    set {}
  }

  @objc var propertyFromHeader4: CInt {
    // OK, provides an implementation with a computed property
    get { return 1 }
    set {}
  }

  @objc let propertyFromHeader5: CInt
  // FIXME: bad, needs to be settable

  @objc var propertyFromHeader6: CInt {
    // FIXME: bad, needs a setter
    get { return 1 }
  }

  @objc var propertyFromHeader7: CInt {
    get { return 1 }
  }

  final var propertyFromHeader8: CInt
  // FIXME: Should complain about final not fulfilling the @objc requirement

  @objc var readonlyPropertyFromHeader1: CInt
  // OK, provides an implementation with a stored property that's nonpublicly settable

  @objc var readonlyPropertyFromHeader2: CInt
  // OK, provides an implementation with a stored property that's nonpublicly settable

  @objc var readonlyPropertyFromHeader3: CInt {
    // FIXME: OK, provides an implementation with a computed property
    get { return 1 }
    set {}
  }

  @objc var readonlyPropertyFromHeader4: CInt {
    // OK, provides an implementation with a computed property
    get { return 1 }
    set {}
  }

  @objc let readonlyPropertyFromHeader5: CInt
  // OK, provides an implementation with a stored read-only property

  @objc var readonlyPropertyFromHeader6: CInt {
    // OK, provides an implementation with a computed read-only property
    get { return 1 }
  }

  @objc var propertyNotFromHeader2: CInt
  // OK, provides a nonpublic but ObjC-compatible stored property

  @objc var propertyNotFromHeader3: CInt {
    // OK, provides a nonpublic but ObjC-compatible computed property
    get { return 1 }
    set {}
  }

  final var propertyNotFromHeader4: CInt
  // OK, provides a Swift-only stored property

  @objc final var propertyNotFromHeader5: CInt
  // OK, @objc final is weird but supported, not a member impl

  override open func superclassMethod(_: CInt) {
    // OK
  }

  override open var superclassProperty: CInt {
    get {
      // OK
    }
    set {
      // OK
    }
  }

  override public init(fromSuperclass v: CInt) {
    // OK
    super.init(fromSuperclass: v)
  }

  override public init(fromSuperclass2 v: CInt) {
    // OK
    super.init(fromSuperclass2: v)
  }
}

// FIXME: Should complain about categoryMethodFromHeader4:
@_objcImplementation(PresentAdditions) extension ObjCClass {
  @objc func categoryMethod(fromHeader3: CInt) {
    // OK
  }

  @objc func categoryMethod(fromHeader1: CInt) {
    // OK, provides an implementation for the header's method.
  }

  @objc func categoryMethod(fromHeader2: CInt) {
    // OK, provides an implementation for the header's method.
  }

  @objc fileprivate func categoryMethodNot(fromHeader1: CInt) {
    // OK, declares a new @objc dynamic method.
  }

  final func categoryMethodNot(fromHeader2: CInt) {
    // OK, declares a new Swift method.
  }

  private func categoryMethodNot(fromHeader3: CInt) {
    // OK
  }

  @objc var categoryPropertyFromHeader2: CInt {
    get { return 1 }
    set {}
  }

  @objc var categoryPropertyFromHeader3: CInt {
    // OK, provides an implementation with a computed property
    get { return 1 }
    set {}
  }

  @objc var categoryPropertyFromHeader4: CInt {
    // OK, provides an implementation with a computed property
    get { return 1 }
    set {}
  }
}

@objc class SwiftClass {}

func usesAreNotAmbiguous(obj: ObjCClass) {
  obj.method(fromHeader1: 1)
  obj.method(fromHeader2: 2)
  obj.method(fromHeader3: 3)
  obj.method(fromHeader4: 4)

  obj.methodNot(fromHeader1: 1)
  obj.methodNot(fromHeader2: 2)

  obj.categoryMethod(fromHeader1: 1)
  obj.categoryMethod(fromHeader2: 2)
  obj.categoryMethod(fromHeader3: 3)
  obj.categoryMethod(fromHeader4: 4)

  obj.categoryMethodNot(fromHeader1: 1)
  obj.categoryMethodNot(fromHeader2: 2)
}
