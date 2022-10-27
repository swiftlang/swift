// RUN: %target-typecheck-verify-swift -import-objc-header %S/Inputs/objc_implementation.h
// REQUIRES: objc_interop

// FIXME: Should complain about method(fromHeader4:) and propertyFromHeader9
@_objcImplementation extension ObjCClass {
  // expected-note@-1 {{previously implemented by extension here}}

  func method(fromHeader1: CInt) {
    // FIXME: OK, provides an implementation for the header's method.
    // expected-error@-2 {{instance method 'method(fromHeader1:)' does not match any instance method declared in the headers for 'ObjCClass'; did you use the instance method's Swift name?}}
    // expected-note@-3 {{add '@objc' to define an Objective-C-compatible instance method not declared in the header}} {{3-3=@objc }}
    // expected-note@-4 {{add 'final' to define a Swift instance method that cannot be overridden}} {{3-3=final }}
  }

  @objc func method(fromHeader2: CInt) {
    // OK, provides an implementation for the header's method.
  }

  func categoryMethod(fromHeader3: CInt) {
    // FIXME: Should complain about the wrong category
    // expected-error@-2 {{instance method 'categoryMethod(fromHeader3:)' does not match any instance method declared in the headers for 'ObjCClass'; did you use the instance method's Swift name?}}
    // expected-note@-3 {{add '@objc' to define an Objective-C-compatible instance method not declared in the header}} {{3-3=@objc }}
    // expected-note@-4 {{add 'final' to define a Swift instance method that cannot be overridden}} {{3-3=final }}
  }

  @objc fileprivate func methodNot(fromHeader1: CInt) {
    // OK, declares a new @objc dynamic method.
  }

  final func methodNot(fromHeader2: CInt) {
    // OK, declares a new Swift method.
  }

  func methodNot(fromHeader3: CInt) {
    // expected-error@-1 {{instance method 'methodNot(fromHeader3:)' does not match any instance method declared in the headers for 'ObjCClass'; did you use the instance method's Swift name?}}
    // expected-note@-2 {{add '@objc' to define an Objective-C-compatible instance method not declared in the header}} {{3-3=@objc }}
    // expected-note@-3 {{add 'final' to define a Swift instance method that cannot be overridden}} {{3-3=final }}
  }

  var propertyFromHeader1: CInt
  // FIXME: OK, provides an implementation with a stored property
  // expected-error@-2 {{property 'propertyFromHeader1' does not match any property declared in the headers for 'ObjCClass'; did you use the property's Swift name?}}
  // expected-note@-3 {{add '@objc' to define an Objective-C-compatible property not declared in the header}} {{3-3=@objc }}
  // expected-note@-4 {{add 'final' to define a Swift property that cannot be overridden}} {{3-3=final }}

  @objc var propertyFromHeader2: CInt
  // OK, provides an implementation with a stored property

  var propertyFromHeader3: CInt {
    // FIXME: OK, provides an implementation with a computed property
    // expected-error@-2 {{property 'propertyFromHeader3' does not match any property declared in the headers for 'ObjCClass'; did you use the property's Swift name?}}
    // expected-note@-3 {{add '@objc' to define an Objective-C-compatible property not declared in the header}} {{3-3=@objc }}
    // expected-note@-4 {{add 'final' to define a Swift property that cannot be overridden}} {{3-3=final }}
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

  final var propertyFromHeader8: CInt
  // FIXME: Should complain about final not fulfilling the @objc requirement

  var readonlyPropertyFromHeader1: CInt
  // FIXME: OK, provides an implementation with a stored property that's nonpublicly settable
  // expected-error@-2 {{property 'readonlyPropertyFromHeader1' does not match any property declared in the headers for 'ObjCClass'; did you use the property's Swift name?}}
  // expected-note@-3 {{add '@objc' to define an Objective-C-compatible property not declared in the header}} {{3-3=@objc }}
  // expected-note@-4 {{add 'final' to define a Swift property that cannot be overridden}} {{3-3=final }}

  @objc var readonlyPropertyFromHeader2: CInt
  // OK, provides an implementation with a stored property that's nonpublicly settable

  var readonlyPropertyFromHeader3: CInt {
    // FIXME: OK, provides an implementation with a computed property
    // expected-error@-2 {{property 'readonlyPropertyFromHeader3' does not match any property declared in the headers for 'ObjCClass'; did you use the property's Swift name?}}
    // expected-note@-3 {{add '@objc' to define an Objective-C-compatible property not declared in the header}} {{3-3=@objc }}
    // expected-note@-4 {{add 'final' to define a Swift property that cannot be overridden}} {{3-3=final }}
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

  var propertyNotFromHeader1: CInt
  // expected-error@-1 {{property 'propertyNotFromHeader1' does not match any property declared in the headers for 'ObjCClass'; did you use the property's Swift name?}}
  // expected-note@-2 {{add '@objc' to define an Objective-C-compatible property not declared in the header}} {{3-3=@objc }}
  // expected-note@-3 {{add 'final' to define a Swift property that cannot be overridden}} {{3-3=final }}

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

  override public init(fromSuperclass _: CInt) {
    // OK
  }
}

// FIXME: Should complain about categoryMethodFromHeader4:
@_objcImplementation(PresentAdditions) extension ObjCClass {
  // expected-note@-1 {{previously implemented by extension here}}

  func method(fromHeader3: CInt) {
    // FIXME: Should complain about wrong category
    // expected-error@-2 {{instance method 'method(fromHeader3:)' does not match any instance method declared in the headers for 'ObjCClass'; did you use the instance method's Swift name?}}
    // expected-note@-3 {{add '@objc' to define an Objective-C-compatible instance method not declared in the header}} {{3-3=@objc }}
    // expected-note@-4 {{add 'final' to define a Swift instance method that cannot be overridden}} {{3-3=final }}
  }

  var propertyFromHeader7: CInt {
    // FIXME: Should complain about wrong category
    // expected-error@-2 {{property 'propertyFromHeader7' does not match any property declared in the headers for 'ObjCClass'; did you use the property's Swift name?}}
    // expected-note@-3 {{add '@objc' to define an Objective-C-compatible property not declared in the header}} {{3-3=@objc }}
    // expected-note@-4 {{add 'final' to define a Swift property that cannot be overridden}} {{3-3=final }}
    get { return 1 }
  }

  func categoryMethod(fromHeader1: CInt) {
    // FIXME: OK, provides an implementation for the header's method.
    // expected-error@-2 {{instance method 'categoryMethod(fromHeader1:)' does not match any instance method declared in the headers for 'ObjCClass'; did you use the instance method's Swift name?}}
    // expected-note@-3 {{add '@objc' to define an Objective-C-compatible instance method not declared in the header}} {{3-3=@objc }}
    // expected-note@-4 {{add 'final' to define a Swift instance method that cannot be overridden}} {{3-3=final }}
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

  func categoryMethodNot(fromHeader3: CInt) {
    // expected-error@-1 {{instance method 'categoryMethodNot(fromHeader3:)' does not match any instance method declared in the headers for 'ObjCClass'; did you use the instance method's Swift name?}}
    // expected-note@-2 {{add '@objc' to define an Objective-C-compatible instance method not declared in the header}} {{3-3=@objc }}
    // expected-note@-3 {{add 'final' to define a Swift instance method that cannot be overridden}} {{3-3=final }}
  }

  var categoryPropertyFromHeader1: CInt
  // expected-error@-1 {{extensions must not contain stored properties}}
  // FIXME: expected-error@-2 {{property 'categoryPropertyFromHeader1' does not match any property declared in the headers for 'ObjCClass'; did you use the property's Swift name?}}
  // FIXME: expected-note@-3 {{add '@objc' to define an Objective-C-compatible property not declared in the header}} {{3-3=@objc }}
  // FIXME: expected-note@-4 {{add 'final' to define a Swift property that cannot be overridden}} {{3-3=final }}

  @objc var categoryPropertyFromHeader2: CInt
  // expected-error@-1 {{extensions must not contain stored properties}}

  var categoryPropertyFromHeader3: CInt {
    // FIXME: OK, provides an implementation with a computed property
    // expected-error@-2 {{property 'categoryPropertyFromHeader3' does not match any property declared in the headers for 'ObjCClass'; did you use the property's Swift name?}}
    // expected-note@-3 {{add '@objc' to define an Objective-C-compatible property not declared in the header}} {{3-3=@objc }}
    // expected-note@-4 {{add 'final' to define a Swift property that cannot be overridden}} {{3-3=final }}
    get { return 1 }
    set {}
  }

  @objc var categoryPropertyFromHeader4: CInt {
    // OK, provides an implementation with a computed property
    get { return 1 }
    set {}
  }
}

@_objcImplementation extension ObjCClass {}
// expected-error@-1 {{duplicate implementation of Objective-C class 'ObjCClass'}}

@_objcImplementation(PresentAdditions) extension ObjCClass {}
// expected-error@-1 {{duplicate implementation of Objective-C category 'PresentAdditions' on class 'ObjCClass'}}

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
