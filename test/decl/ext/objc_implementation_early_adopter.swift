// RUN: %target-typecheck-verify-swift -import-objc-header %S/Inputs/objc_implementation.h -enable-experimental-feature ObjCImplementation -enable-experimental-feature CImplementation -target %target-stable-abi-triple
// REQUIRES: objc_interop
// REQUIRES: swift_feature_CImplementation
// REQUIRES: swift_feature_ObjCImplementation

protocol EmptySwiftProto {}

@_objcImplementation extension ObjCClass: EmptySwiftProto, EmptyObjCProto {
  // expected-note@-1 {{previously implemented here}}
  // expected-warning@-2 {{extension for main class interface should provide implementation for instance method 'method(fromHeader4:)'}}
  // expected-warning@-3 {{extension for main class interface should provide implementation for property 'propertyFromHeader9'}}
  // FIXME: give better diagnostic expected-warning@-4 {{extension for main class interface should provide implementation for property 'propertyFromHeader8'}}
  // FIXME: give better diagnostic expected-warning@-5 {{extension for main class interface should provide implementation for property 'propertyFromHeader7'}}
  // FIXME: give better diagnostic expected-warning@-6 {{extension for main class interface should provide implementation for instance method 'method(fromHeader3:)'}}
  // expected-warning@-7 {{'@objc @implementation' extension cannot add conformance to 'EmptySwiftProto'; add this conformance with an ordinary extension}}
  // expected-warning@-8 {{'@objc @implementation' extension cannot add conformance to 'EmptyObjCProto'; add this conformance in the Objective-C header}}
  // expected-warning@-9 {{extension for main class interface should provide implementation for instance method 'extensionMethod(fromHeader2:)'}}

  func method(fromHeader1: CInt) {
    // OK, provides an implementation for the header's method.
  }

  @objc func method(fromHeader2: CInt) {
    // OK, provides an implementation for the header's method.
  }

  func categoryMethod(fromHeader3: CInt) {
    // FIXME: should emit expected-DISABLED-error@-1 {{instance method 'categoryMethod(fromHeader3:)' should be implemented in extension for category 'PresentAdditions', not main class interface}}
    // FIXME: expected-warning@-2 {{instance method 'categoryMethod(fromHeader3:)' does not match any instance method declared in the headers for 'ObjCClass'; did you use the instance method's Swift name?; this will become an error after adopting '@implementation'}}
    // FIXME: expected-note@-3 {{add 'private' or 'fileprivate' to define an Objective-C-compatible instance method not declared in the header}} {{3-3=private }}
    // FIXME: expected-note@-4 {{add 'final' to define a Swift-only instance method that cannot be overridden}} {{3-3=final }}
  }

  @objc fileprivate func methodNot(fromHeader1: CInt) {
    // OK, declares a new @objc dynamic method.
  }

  final func methodNot(fromHeader2: CInt) {
    // OK, declares a new Swift method.
  }

  func methodNot(fromHeader3: CInt) {
    // expected-warning@-1 {{instance method 'methodNot(fromHeader3:)' does not match any instance method declared in the headers for 'ObjCClass'; did you use the instance method's Swift name?; this will become an error after adopting '@implementation'}}
    // expected-note@-2 {{add 'private' or 'fileprivate' to define an Objective-C-compatible instance method not declared in the header}} {{3-3=private }}
    // expected-note@-3 {{add 'final' to define a Swift-only instance method that cannot be overridden}} {{3-3=final }}
  }

  var methodFromHeader5: CInt
  // expected-warning@-1 {{property 'methodFromHeader5' does not match the instance method declared by the header}}

  func method(fromHeader6: Double) {
    // expected-warning@-1 {{instance method 'method(fromHeader6:)' of type '(Double) -> ()' does not match type '(Int32) -> Void' declared by the header}}
  }

  var propertyFromHeader1: CInt
  // OK, provides an implementation with a stored property

  @objc var propertyFromHeader2: CInt
  // OK, provides an implementation with a stored property

  var propertyFromHeader3: CInt {
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
  // expected-warning@-1 {{property 'propertyFromHeader5' should be settable to match the settable property declared by the header}}

  @objc var propertyFromHeader6: CInt {
    // expected-warning@-1 {{property 'propertyFromHeader6' should be settable to match the settable property declared by the header}}
    get { return 1 }
  }

  final var propertyFromHeader8: CInt
  // FIXME: Should complain about final not fulfilling the @objc requirement

  func propertyFromHeader10() -> CInt {
    // expected-warning@-1 {{instance method 'propertyFromHeader10()' does not match the property declared by the header}}
    return 1
  }

  var propertyFromHeader11: Float
  // expected-warning@-1 {{property 'propertyFromHeader11' of type 'Float' does not match type 'Int32' declared by the header}}

  var readonlyPropertyFromHeader1: CInt
  // OK, provides an implementation with a stored property that's nonpublicly settable

  @objc var readonlyPropertyFromHeader2: CInt
  // OK, provides an implementation with a stored property that's nonpublicly settable

  var readonlyPropertyFromHeader3: CInt {
    // OK, provides an implementation with a computed property
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

  internal var propertyNotFromHeader1: CInt
  // expected-warning@-1 {{property 'propertyNotFromHeader1' does not match any property declared in the headers for 'ObjCClass'; did you use the property's Swift name?; this will become an error after adopting '@implementation'}}
  // expected-note@-2 {{add 'private' or 'fileprivate' to define an Objective-C-compatible property not declared in the header}} {{3-11=private}}
  // expected-note@-3 {{add 'final' to define a Swift-only property that cannot be overridden}} {{3-3=final }}

  @objc private var propertyNotFromHeader2: CInt
  // OK, provides a nonpublic but ObjC-compatible stored property

  @objc fileprivate var propertyNotFromHeader3: CInt {
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

  @objc(initFromProtocol1:)
  required public init?(fromProtocol1: CInt) {
    // OK
  }

  @objc(initFromProtocol2:)
  public init?(fromProtocol2: CInt) {
    // expected-warning@-1 {{initializer 'init(fromProtocol2:)' should be 'required' to match initializer declared by the header}} {{3-3=required }}
  }

  @objc(initNotFromProtocol:)
  required public init?(notFromProtocol: CInt) {
    // expected-warning@-1 {{initializer 'init(notFromProtocol:)' should not be 'required' to match initializer declared by the header}} {{3-12=}}
  }

  class func classMethod1(_: CInt) {
    // OK
  }

  func classMethod2(_: CInt) {
    // expected-warning@-1 {{instance method 'classMethod2' does not match class method declared in header; this will become an error after adopting '@implementation'}} {{3-3=class }}
  }

  class func classMethod3(_: Float) {
    // expected-warning@-1 {{class method 'classMethod3' of type '(Float) -> ()' does not match type '(Int32) -> Void' declared by the header}}
  }

  func instanceMethod1(_: CInt) {
    // OK
  }

  class func instanceMethod2(_: CInt) {
    // expected-warning@-1 {{class method 'instanceMethod2' does not match instance method declared in header; this will become an error after adopting '@implementation'}} {{3-9=}}
  }

  public init(notFromHeader1: CInt) {
    // expected-warning@-1 {{initializer 'init(notFromHeader1:)' does not match any initializer declared in the headers for 'ObjCClass'; did you use the initializer's Swift name?}}
    // expected-note@-2 {{add 'private' or 'fileprivate' to define an Objective-C-compatible initializer not declared in the header}} {{3-9=private}}
    // expected-note@-3 {{add '@nonobjc' to define a Swift-only initializer}} {{3-3=@nonobjc }}
  }

  public required init(notFromHeader2: CInt) {
    // expected-warning@-1 {{initializer 'init(notFromHeader2:)' does not match any initializer declared in the headers for 'ObjCClass'; did you use the initializer's Swift name?}}
    // expected-note@-2 {{add 'private' or 'fileprivate' to define an Objective-C-compatible initializer not declared in the header}} {{3-9=private}}
    // expected-note@-3 {{add '@nonobjc' to define a Swift-only initializer}} {{3-3=@nonobjc }}
  }

  public convenience init(notFromHeader3: CInt) {
    // expected-warning@-1 {{initializer 'init(notFromHeader3:)' does not match any initializer declared in the headers for 'ObjCClass'; did you use the initializer's Swift name?}}
    // expected-note@-2 {{add 'private' or 'fileprivate' to define an Objective-C-compatible initializer not declared in the header}} {{3-9=private}}
    // expected-note@-3 {{add '@nonobjc' to define a Swift-only initializer}} {{3-3=@nonobjc }}
  }

  @nonobjc public init(notFromHeader4: CInt) {
    // expected-warning@-1 {{initializer 'init(notFromHeader4:)' is not valid in an '@objc @implementation' extension because Objective-C subclasses must be able to override designated initializers}}
    // expected-note@-2 {{add 'convenience' keyword to make this a convenience initializer}} {{12-12=convenience }}
  }

  @nonobjc public required init(notFromHeader5: CInt) {
    // expected-warning@-1 {{initializer 'init(notFromHeader5:)' is not valid in an '@objc @implementation' extension because Objective-C subclasses must be able to override required initializers}}
    // expected-note@-2 {{replace 'required' keyword with 'convenience' to make this a convenience initializer}} {{19-27=convenience}}
  }

  @nonobjc public convenience init(notFromHeader6: CInt) {
    // OK
  }

  @objc func extensionMethod(fromHeader1: CInt) {
    // OK
  }

  @objc(copyWithZone:) func copy(with zone: NSZone?) -> Any {
    // OK
    return self
  }

  // rdar://122280735 - crash when the parameter of a block property needs @escaping
  let rdar122280735: (() -> ()) -> Void = { _ in }
  // expected-warning@-1 {{property 'rdar122280735' of type '(() -> ()) -> Void' does not match type '(@escaping () -> Void) -> Void' declared by the header}}
}

@_objcImplementation(PresentAdditions) extension ObjCClass {
  // expected-note@-1 {{'PresentAdditions' previously declared here}}
  // expected-warning@-2 {{extension for category 'PresentAdditions' should provide implementation for instance method 'categoryMethod(fromHeader4:)'; this will become an error after adopting '@implementation'}}
  // FIXME: give better diagnostic expected-warning@-3 {{extension for category 'PresentAdditions' should provide implementation for instance method 'categoryMethod(fromHeader3:)'; this will become an error after adopting '@implementation'}}

  func method(fromHeader3: CInt) {
    // FIXME: should emit expected-DISABLED-error@-1 {{instance method 'method(fromHeader3:)' should be implemented in extension for main class interface, not category 'PresentAdditions'}}
    // FIXME: expected-warning@-2 {{instance method 'method(fromHeader3:)' does not match any instance method declared in the headers for 'ObjCClass'; did you use the instance method's Swift name?; this will become an error after adopting '@implementation'}}
    // FIXME: expected-note@-3 {{add 'private' or 'fileprivate' to define an Objective-C-compatible instance method not declared in the header}} {{3-3=private }}
    // FIXME: expected-note@-4 {{add 'final' to define a Swift-only instance method that cannot be overridden}} {{3-3=final }}
  }

  var propertyFromHeader7: CInt {
    // FIXME: should emit expected-DISABLED-error@-1 {{property 'propertyFromHeader7' should be implemented in extension for main class interface, not category 'PresentAdditions'}}
    // FIXME: expected-warning@-2 {{property 'propertyFromHeader7' does not match any property declared in the headers for 'ObjCClass'; did you use the property's Swift name?; this will become an error after adopting '@implementation'}}
    // FIXME: expected-note@-3 {{add 'private' or 'fileprivate' to define an Objective-C-compatible property not declared in the header}} {{3-3=private }}
    // FIXME: expected-note@-4 {{add 'final' to define a Swift-only property that cannot be overridden}} {{3-3=final }}
    get { return 1 }
  }

  func categoryMethod(fromHeader1: CInt) {
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

  func categoryMethodNot(fromHeader3: CInt) {
    // expected-warning@-1 {{instance method 'categoryMethodNot(fromHeader3:)' does not match any instance method declared in the headers for 'ObjCClass'; did you use the instance method's Swift name?; this will become an error after adopting '@implementation'}}
    // expected-note@-2 {{add 'private' or 'fileprivate' to define an Objective-C-compatible instance method not declared in the header}} {{3-3=private }}
    // expected-note@-3 {{add 'final' to define a Swift-only instance method that cannot be overridden}} {{3-3=final }}
  }

  var categoryPropertyFromHeader1: CInt
  // expected-error@-1 {{extensions must not contain stored properties}}

  @objc var categoryPropertyFromHeader2: CInt
  // expected-error@-1 {{extensions must not contain stored properties}}

  var categoryPropertyFromHeader3: CInt {
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

@_objcImplementation(SwiftNameTests) extension ObjCClass {
  // expected-warning@-1 {{extension for category 'SwiftNameTests' should provide implementation for instance method 'methodSwiftName6B()'; this will become an error after adopting '@implementation'}}

  func methodSwiftName1() {
    // expected-warning@-1 {{selector 'methodSwiftName1' for instance method 'methodSwiftName1()' not found in header; did you mean 'methodObjCName1'?; this will become an error after adopting '@implementation'}} {{3-3=@objc(methodObjCName1) }}
  }

  @objc(methodObjCName2) func methodSwiftName2() {
    // OK
  }

  func methodObjCName3() {
    // expected-warning@-1 {{selector 'methodObjCName3' used in header by instance method with a different name; did you mean 'methodSwiftName3()'?}} {{8-23=methodSwiftName3}} {{3-3=@objc(methodObjCName3) }}
    // FIXME: probably needs an @objc too, since the name is not explicit
  }

  @objc(methodWrongObjCName4) func methodSwiftName4() {
    // expected-warning@-1 {{selector 'methodWrongObjCName4' for instance method 'methodSwiftName4()' not found in header; did you mean 'methodObjCName4'?; this will become an error after adopting '@implementation'}} {{9-29=methodObjCName4}}
  }

  @objc(methodObjCName5) func methodWrongSwiftName5() {
    // expected-warning@-1 {{selector 'methodObjCName5' used in header by instance method with a different name; did you mean 'methodSwiftName5()'?}} {{31-52=methodSwiftName5}}
  }

  @objc(methodObjCName6A) func methodSwiftName6B() {
    // expected-warning@-1 {{selector 'methodObjCName6A' used in header by instance method with a different name; did you mean 'methodSwiftName6A()'?}} {{32-49=methodSwiftName6A}}
  }
}

@_objcImplementation(AmbiguousMethods) extension ObjCClass {
  // expected-warning@-1 {{found multiple implementations that could match instance method 'ambiguousMethod4(with:)' with selector 'ambiguousMethod4WithCInt:'; this will become an error after adopting '@implementation'}}

  @objc func ambiguousMethod1(with: CInt) {
    // expected-warning@-1 {{instance method 'ambiguousMethod1(with:)' could match several different members declared in the header; this will become an error after adopting '@implementation'}}
    // expected-note@-2 {{instance method 'ambiguousMethod1(with:)' (with selector 'ambiguousMethod1WithCInt:') is a potential match; insert '@objc(ambiguousMethod1WithCInt:)' to use it}} {{8-8=(ambiguousMethod1WithCInt:)}}
    // expected-note@-3 {{instance method 'ambiguousMethod1(with:)' (with selector 'ambiguousMethod1WithCChar:') is a potential match; insert '@objc(ambiguousMethod1WithCChar:)' to use it}} {{8-8=(ambiguousMethod1WithCChar:)}}
  }

  func ambiguousMethod1(with: CChar) {
    // expected-warning@-1 {{instance method 'ambiguousMethod1(with:)' could match several different members declared in the header; this will become an error after adopting '@implementation'}}
    // expected-note@-2 {{instance method 'ambiguousMethod1(with:)' (with selector 'ambiguousMethod1WithCInt:') is a potential match; insert '@objc(ambiguousMethod1WithCInt:)' to use it}} {{3-3=@objc(ambiguousMethod1WithCInt:) }}
    // expected-note@-3 {{instance method 'ambiguousMethod1(with:)' (with selector 'ambiguousMethod1WithCChar:') is a potential match; insert '@objc(ambiguousMethod1WithCChar:)' to use it}} {{3-3=@objc(ambiguousMethod1WithCChar:) }}
  }

  @objc(ambiguousMethod2WithCInt:) func ambiguousMethod2(with: CInt) {
    // OK, matches -ambiguousMethod2WithCInt: because of explicit @objc
  }

  func ambiguousMethod2(with: CChar) {
    // FIXME: OK, matches -ambiguousMethod2WithCChar: because the WithCInt: variant has been eliminated
    // FIXME: expected-warning@-2 {{selector 'ambiguousMethod2With:' for instance method 'ambiguousMethod2(with:)' not found in header; did you mean 'ambiguousMethod2WithCChar:'?; this will become an error after adopting '@implementation'}}
  }

  func ambiguousMethod3(with: CInt) {
    // expected-warning@-1 {{instance method 'ambiguousMethod3(with:)' could match several different members declared in the header; this will become an error after adopting '@implementation'}}
    // expected-note@-2 {{instance method 'ambiguousMethod3(with:)' (with selector 'ambiguousMethod3WithCInt:') is a potential match; insert '@objc(ambiguousMethod3WithCInt:)' to use it}} {{3-3=@objc(ambiguousMethod3WithCInt:) }}
    // expected-note@-3 {{instance method 'ambiguousMethod3(with:)' (with selector 'ambiguousMethod3WithCChar:') is a potential match; insert '@objc(ambiguousMethod3WithCChar:)' to use it}} {{3-3=@objc(ambiguousMethod3WithCChar:) }}
  }

  @objc func ambiguousMethod4(with: CInt) {
    // expected-note@-1 {{instance method 'ambiguousMethod4(with:)' is a potential match; insert '@objc(ambiguousMethod4WithCInt:)' to use it}} {{8-8=(ambiguousMethod4WithCInt:)}}
  }

  func ambiguousMethod4(with: CChar) {
    // expected-note@-1 {{instance method 'ambiguousMethod4(with:)' is a potential match; insert '@objc(ambiguousMethod4WithCInt:)' to use it}} {{3-3=@objc(ambiguousMethod4WithCInt:) }}
  }
}

@_objcImplementation(Effects) extension ObjCClass {
  @available(SwiftStdlib 5.1, *)
  public func doSomethingAsynchronous() async throws -> Any {
    return self
  }

  @available(SwiftStdlib 5.1, *)
  public func doSomethingElseAsynchronous() async -> Any {
    return self
  }

  public func doSomethingFunAndAsynchronous(completionHandler: @escaping (Any?, Error?) -> Void) {
  }

  @available(SwiftStdlib 5.1, *)
  @objc(doSomethingOverloadedWithCompletionHandler:)
  public func doSomethingOverloaded() async {}

  @available(*, noasync)
  @objc(doSomethingOverloaded)
  public func doSomethingOverloaded() {}

  @objc(doSomethingThatCanFailWithHandler:error:)
  public func doSomethingThatCanFail(handler: @escaping () -> Void) throws {
    // OK
  }

  @objc(doSomethingElseThatCanFail:handler:)
  public func doSomethingElseThatCanFail(handler: @escaping () -> Void) throws {
    // OK
  }

  @objc(doSomethingThatCanFailWithWeirdParameterWithHandler::)
  public func doSomethingThatCanFailWithWeirdParameter(handler: @escaping () -> Void) throws {
    // expected-warning@-1 {{instance method 'doSomethingThatCanFailWithWeirdParameter(handler:)' does not match the declaration in the header because it uses parameter #1 for the error, not parameter #2; a selector part called 'error:' can control which parameter to use}}
  }

  @objc(doSomethingThatCanFailWithWeirdReturnCodeWithError:)
  public func doSomethingThatCanFailWithWeirdReturnCode() throws {
    // expected-warning@-1 {{instance method 'doSomethingThatCanFailWithWeirdReturnCode()' does not match the declaration in the header because it indicates an error by returning zero, rather than by returning non-zero}}
  }
}

@_objcImplementation(Conformance) extension ObjCClass {
  // expected-warning@-1 {{extension for category 'Conformance' should provide implementation for instance method 'requiredMethod2()'; this will become an error after adopting '@implementation'}}
  // no-error concerning 'optionalMethod2()'

  func requiredMethod1() {}

  func optionalMethod1() {}
}

@_objcImplementation(TypeMatchOptionality) extension ObjCClass {
  func nullableResultAndArg(_: Any?) -> Any? { fatalError() } // no-error
  func nonnullResultAndArg(_: Any) -> Any { fatalError() } // no-error
  func nullUnspecifiedResultAndArg(_: Any!) -> Any! { fatalError() } // no-error

  func nonnullResult1() -> Any { fatalError() } // no-error
  func nonnullResult2() -> Any? { fatalError() } // expected-warning {{instance method 'nonnullResult2()' of type '() -> Any?' does not match type '() -> Any' declared by the header}}
  func nonnullResult3() -> Any! { fatalError() } // no-error (special case)

  func nonnullArgument1(_: Any) {} // no-error
  func nonnullArgument2(_: Any?) {} // expected-warning {{instance method 'nonnullArgument2' of type '(Any?) -> ()' does not match type '(Any) -> Void' declared by the header}}
  func nonnullArgument3(_: Any!) {} // no-error (special case)

  func nullableResult() -> Any { fatalError() } // expected-warning {{instance method 'nullableResult()' of type '() -> Any' does not match type '() -> Any?' declared by the header}}
  func nullableArgument(_: Any) {} // expected-warning {{instance method 'nullableArgument' of type '(Any) -> ()' does not match type '(Any?) -> Void' declared by the header}}
}

@_objcImplementation(TypeMatchOptionalityInvalid) extension ObjCClass {
  func nonPointerResult() -> CInt! { fatalError() } // expected-warning{{method cannot be in an @objc @implementation extension of a class (without final or @nonobjc) because its result type cannot be represented in Objective-C}}
  func nonPointerArgument(_: CInt!) {} // expected-warning {{method cannot be in an @objc @implementation extension of a class (without final or @nonobjc) because the type of the parameter cannot be represented in Objective-C}}
}

@_objcImplementation(InvalidMembers) extension ObjCClass {
  // expected-warning@-1 {{extension for category 'InvalidMembers' should provide implementation for instance method 'unimplementedMember()'}}

  func nonObjCMethod(_: EmptySwiftProto) {
    // expected-warning@-1 {{method cannot be in an @objc @implementation extension of a class (without final or @nonobjc) because the type of the parameter cannot be represented in Objective-C}}
    // expected-note@-2 {{protocol-constrained type containing protocol 'EmptySwiftProto' cannot be represented in Objective-C}}
  }

  private func privateNonObjCMethod(_: EmptySwiftProto) {
    // expected-warning@-1 {{instance method 'privateNonObjCMethod' will become implicitly '@objc' after adopting '@objc @implementation'; add 'final' to keep the current behavior}} {{3-3=final }}
    // expected-note@-2 {{add '@objc' to expose this instance method to Objective-C}} {{3-3=@objc }}
  }

  override public static func superclassClassMethod() {
    // rdar://136113393: `static override` could make a non-`@objc` override
    // of an `@objc` member when using new syntax. (We would get a warning here
    // if that bug were still in place.)
  }
}

@objc @_objcImplementation extension ObjCClassWithWeirdSwiftAttributeCombo {
  static func staticFnPreviouslyTreatedAsAtObjcExtensionMember() {
    // expected-warning@-1 {{static method 'staticFnPreviouslyTreatedAsAtObjcExtensionMember()' will no longer be implicitly '@objc' after adopting '@objc @implementation'; add '@objc' to keep the current behavior}} {{3-3=@objc }}
    // expected-note@-2 {{add 'final' to define a Swift-only static method that cannot be overridden}} {{3-3=final }}
  }
}

// Intentionally using `@_objcImplementation` for this test; do not upgrade!
@_objcImplementation(EmptyCategory) extension ObjCClass {
  // expected-warning@-1 {{'@_objcImplementation' is deprecated; use '@implementation' instead}} {{1-36=@implementation}} {{1-1=@objc(EmptyCategory) }}
}

@_objcImplementation extension ObjCImplSubclass {
  // expected-warning@-1 {{'@_objcImplementation' is deprecated; use '@implementation' instead}} {{1-21=@implementation}} {{1-1=@objc }}
  @objc(initFromProtocol1:)
    required public init?(fromProtocol1: CInt) {
      // OK
    }
}

@_objcImplementation extension ObjCBasicInitClass {
  // expected-warning@-1 {{'@_objcImplementation' is deprecated; use '@implementation' instead}} {{1-21=@implementation}} {{1-1=@objc }}
  init() {
    // OK
  }
}

@_objcImplementation extension ObjCClass {}
// expected-error@-1 {{duplicate implementation of imported class 'ObjCClass'}}

@_objcImplementation(PresentAdditions) extension ObjCClass {}
// expected-warning@-1 {{extension with Objective-C category name 'PresentAdditions' conflicts with previous extension with the same category name; this is an error in the Swift 6 language mode}}

@_objcImplementation(MissingAdditions) extension ObjCClass {}
// expected-error@-1 {{could not find category 'MissingAdditions' on Objective-C class 'ObjCClass'; make sure your umbrella or bridging header imports the header that declares it}}
// expected-note@-2 {{remove arguments to implement the main '@interface' for this class}} {{21-39=}}

@_objcImplementation extension ObjCStruct {}
// expected-error@-1 {{'@objc' can only be applied to an extension of a class}}

@_objcImplementation(CantWork) extension ObjCStruct {}
// expected-error@-1 {{'@objc' can only be applied to an extension of a class}}

@objc class SwiftClass {}
// expected-note@-1 2 {{'SwiftClass' declared here}}

@_objcImplementation extension SwiftClass {}
// expected-error@-1 {{'@objc @implementation' cannot be used to extend class 'SwiftClass' because it was defined by a Swift 'class' declaration, not an imported Objective-C '@interface' declaration}} {{1-22=}}

@_objcImplementation(WTF) extension SwiftClass {} // expected
// expected-error@-1 {{'@objc @implementation' cannot be used to extend class 'SwiftClass' because it was defined by a Swift 'class' declaration, not an imported Objective-C '@interface' declaration}} {{1-27=}}

@_objcImplementation extension ObjCImplRootClass {
  // expected-error@-1 {{'@objc @implementation' cannot be used to implement root class 'ObjCImplRootClass'; declare its superclass in the header}}
}

@_objcImplementation extension ObjCImplGenericClass {
  // expected-error@-1 {{'@objc @implementation' cannot be used to implement generic class 'ObjCImplGenericClass'}}
}

//
// @_cdecl for global functions
//

@_objcImplementation @_cdecl("CImplFunc1")
func CImplFunc1(_: Int32) {
  // OK
}

@_objcImplementation(BadCategory) @_cdecl("CImplFunc2")
func CImplFunc2(_: Int32) {
  // expected-error@-2 {{global function 'CImplFunc2' does not belong to an Objective-C category; remove the category name from this attribute}} {{21-34=}}
}

@_objcImplementation @_cdecl("CImplFuncMissing")
func CImplFuncMissing(_: Int32) {
  // expected-error@-2 {{could not find imported function 'CImplFuncMissing' matching global function 'CImplFuncMissing'; make sure your umbrella or bridging header imports the header that declares it}}
}

@_objcImplementation @_cdecl("CImplFuncMismatch1")
func CImplFuncMismatch1(_: Float) {
  // expected-warning@-1 {{global function 'CImplFuncMismatch1' of type '(Float) -> ()' does not match type '(Int32) -> Void' declared by the header}}
}

@_objcImplementation @_cdecl("CImplFuncMismatch2")
func CImplFuncMismatch2(_: Int32) -> Float {
  // expected-warning@-1 {{global function 'CImplFuncMismatch2' of type '(Int32) -> Float' does not match type '(Int32) -> Void' declared by the header}}
}

@_objcImplementation @_cdecl("CImplFuncMismatch3")
func CImplFuncMismatch3(_: Any?) {
  // OK
}

@_objcImplementation @_cdecl("CImplFuncMismatch4")
func CImplFuncMismatch4(_: Any) {
  // expected-warning@-1 {{global function 'CImplFuncMismatch4' of type '(Any) -> ()' does not match type '(Any?) -> Void' declared by the header}}
}

@_objcImplementation @_cdecl("CImplFuncMismatch5")
func CImplFuncMismatch5(_: Any) {
  // OK
}

@_objcImplementation @_cdecl("CImplFuncMismatch6")
func CImplFuncMismatch6(_: Any!) {
  // OK, mismatch allowed
}


@_objcImplementation @_cdecl("CImplFuncMismatch3a")
func CImplFuncMismatch3a(_: Int32) -> Any? {
  // OK
}

@_objcImplementation @_cdecl("CImplFuncMismatch4a")
func CImplFuncMismatch4a(_: Int32) -> Any {
  // expected-warning@-1 {{global function 'CImplFuncMismatch4a' of type '(Int32) -> Any' does not match type '(Int32) -> Any?' declared by the header}}
}

@_objcImplementation @_cdecl("CImplFuncMismatch5a")
func CImplFuncMismatch5a(_: Int32) -> Any {
  // OK
}

@_objcImplementation @_cdecl("CImplFuncMismatch6a")
func CImplFuncMismatch6a(_: Int32) -> Any! {
  // OK, mismatch allowed
}

@_objcImplementation @_cdecl("CImplFuncNameMismatch1")
func mismatchedName1(_: Int32) {
  // expected-error@-2 {{could not find imported function 'CImplFuncNameMismatch1' matching global function 'mismatchedName1'; make sure your umbrella or bridging header imports the header that declares it}}
  // FIXME: Improve diagnostic for a partial match.
}

@_objcImplementation @_cdecl("mismatchedName2")
func CImplFuncNameMismatch2(_: Int32) {
  // expected-error@-2 {{could not find imported function 'mismatchedName2' matching global function 'CImplFuncNameMismatch2'; make sure your umbrella or bridging header imports the header that declares it}}
  // FIXME: Improve diagnostic for a partial match.
}

//
// TODO: @_cdecl for global functions imported as computed vars
//
var cImplComputedGlobal1: Int32 {
  @_objcImplementation @_cdecl("CImplGetComputedGlobal1")
  get {
    // FIXME: Lookup for vars isn't working yet
    // expected-error@-3 {{could not find imported function 'CImplGetComputedGlobal1' matching getter for var 'cImplComputedGlobal1'; make sure your umbrella or bridging header imports the header that declares it}}
    return 0
  }

  @_objcImplementation @_cdecl("CImplSetComputedGlobal1")
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
  @_objcImplementation @_cdecl("CImplStructStaticFunc1")
  static func staticFunc1(_: Int32) {
    // FIXME: Add underlying support for this
    // expected-error@-3 {{@_cdecl can only be applied to global functions}}
    // FIXME: Lookup in an enclosing type is not working yet
    // expected-error@-5 {{could not find imported function 'CImplStructStaticFunc1' matching static method 'staticFunc1'; make sure your umbrella or bridging header imports the header that declares it}}
  }
}

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
