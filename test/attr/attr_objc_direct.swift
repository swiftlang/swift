// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// Where '@objcDirect' is accepted and rejected.

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ObjCDirect

import Foundation

// MARK: - Methods and initializers

class ObjCDirectClass: NSObject {
  @objcDirect
  public final func direct() { return }

  @objcDirect
  public final func withParams(x: Int, y: String) { return }

  @objc(RenamedMethod) @objcDirect
  public final func renamed() { return }

  @objcDirect
  public static func staticMethod() -> Int { return 42 }

  @objcDirect
  public func nonFinal() { return }
  // expected-note@-1 {{overridden declaration is here}}
  // expected-note@-2 {{instance method 'nonFinal()' is implicitly 'final' because of '@objcDirect'}}

  @objcDirect
  public final func variadic(format: String, args: CVarArg...) { return }
  // expected-error@-1 {{instance method cannot be marked '@objc' because it has a variadic parameter}}

  @objcDirect
  private final func privateMethod() { return }
  // expected-error@-2 {{an '@objcDirect' instance method must be visible to the whole module}}
}

final class FinalClass: NSObject {
  @objcDirect
  public func implicitlyFinal() { return }

  @objcDirect
  public func implicitlyFinalNoObjC() { return }
}

class ImpliedObjC: NSObject {
  @objcDirect
  final func implied() -> Int { return 1 }

  @objcDirect
  final func impliedWithParams(x: Int) -> String { return "" }

  // The implicit '@objc' is validated, not silently added.
  @objcDirect
  final func notRepresentableParam(x: NotObjCRepresentable) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  @objcDirect
  final func notRepresentableResult() -> (Int, Int) { return (1, 2) }
  // expected-error@-1 {{instance method cannot be marked '@objc' because its result type cannot be represented in Objective-C}}
  // expected-note@-2 {{tuples cannot be represented in Objective-C}}
}

struct NotObjCRepresentable {}

class InitClass: NSObject {
  // init is exempt from the `final` requirement.
  @objcDirect
  init(ok value: Int) { super.init() }

  @objcDirect
  required init(bad value: String) { super.init() }
  // expected-error@-2 {{'@objcDirect' cannot be applied to 'required' initializers}}
}

class DeinitClass: NSObject {
  @objcDirect deinit {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to 'deinit'}}
}

extension ObjCDirectClass {
  @objcDirect
  public final func inExtension() { return }
}

extension ObjCDirectClass {
  @objc
  public func overridable() { return }
  // expected-note@-1 {{overridden declaration is here}}
}

class SubClass: ObjCDirectClass {
  @objcDirect
  public final func notAnOverride() { return }

  @objcDirect
  public final func asyncMethod() async -> Int { return 1 }
  // expected-error@-2 {{'@objcDirect' is not supported with 'async' methods}}

  @objcDirect
  public override final func overridable() { return }
  // expected-error@-2 {{an '@objcDirect' instance method cannot override a superclass declaration}}

  // '@objcDirect' implies 'final'.
  public override func nonFinal() { return }
  // expected-error@-1 {{instance method overrides a 'final' instance method}}
}

@objc
protocol DirectProtocol {
  @objcDirect
  func requirement()
  // expected-error@-2 {{'@objcDirect' cannot be applied to protocol requirements}}
}

// A generic class is never printed to the generated header.
class GenericClass<T>: NSObject {
  @objcDirect
  public final func inGeneric() { return }
  // expected-error@-2 {{'@objcDirect' cannot be applied to members of 'GenericClass', which is not exposed to Objective-C}}
}

@objcDirect
public func globalFunction() {}
// expected-error@-2 {{'@objcDirect' can only be applied to members of a class declared at file scope}}

struct SomeStruct {
  @objcDirect func structMethod() {}
  // expected-error@-1 {{'@objcDirect' can only be applied to members of a class declared at file scope}}
}

enum SomeEnum {
  @objcDirect static func enumMethod() {}
  // expected-error@-1 {{'@objcDirect' can only be applied to members of a class declared at file scope}}
}

// MARK: - Properties and subscripts

class StorageClass: NSObject {
  @objcDirect var stored: Int = 0

  @objcDirect var computed: Int { get { 0 } set {} }

  @objcDirect var readOnly: Int { 0 }

  @objcDirect let constant: Int = 0

  @objc(renamedCount) @objcDirect var renamed: Int = 0

  @objcDirect subscript(i: Int) -> Int { get { i } set {} }

  @objcDirect subscript(key: NSString) -> NSString { NSString() }
}

class AccessorClass: NSObject {
  var onGetter: Int {
    @objcDirect get { 0 }
    // expected-error@-1 {{'@objcDirect' cannot be applied to accessors; apply it to the property to make both accessors direct}}
    set {}
  }

  var onSetter: Int {
    get { 0 }
    @objcDirect set {}
    // expected-error@-1 {{'@objcDirect' cannot be applied to accessors; apply it to the property to make both accessors direct}}
  }

  subscript(i: Int) -> Int {
    @objcDirect get { i }
    // expected-error@-1 {{'@objcDirect' cannot be applied to accessors; apply it to the subscript to make both accessors direct}}
    set {}
  }
}

class DynamicClass: NSObject {
  @objcDirect dynamic var explicitlyDynamic: Int = 0
  // expected-error@-1 {{'@objcDirect' cannot be applied to a 'dynamic' property}}

  // '@NSManaged' implies 'dynamic'.
  @objcDirect @NSManaged var managed: NSString
  // expected-error@-1 {{'@objcDirect' cannot be applied to a 'dynamic' property}}
}

class AccessLevelClass: NSObject {
  @objcDirect private var hidden: Int = 0
  // expected-error@-1 {{an '@objcDirect' property must be visible to the whole module}}

  @objcDirect fileprivate subscript(i: Int) -> Int { i }
  // expected-error@-1 {{an '@objcDirect' subscript must be visible to the whole module}}
}

class Base: NSObject {
  @objc var overridable: Int = 0
  // expected-note@-1 {{overridden declaration is here}}

  @objcDirect var notOverridable: Int = 0
  // expected-note@-1 {{overridden declaration is here}}
  // expected-note@-2 {{property 'notOverridable' is implicitly 'final' because of '@objcDirect'}}

  @objcDirect subscript(i: Int) -> Int { i }
  // expected-note@-1 {{overridden declaration is here}}
  // expected-note@-2 {{subscript 'subscript(_:)' is implicitly 'final' because of '@objcDirect'}}
}

class Derived: Base {
  @objcDirect override var overridable: Int { get { 0 } set {} }
  // expected-error@-1 {{an '@objcDirect' property cannot override a superclass declaration}}

  // '@objcDirect' implies 'final'; reported once, against the storage.
  override var notOverridable: Int { get { 0 } set {} }
  // expected-error@-1 {{property overrides a 'final' property}}

  override subscript(i: Int) -> Int { 0 }
  // expected-error@-1 {{subscript overrides a 'final' subscript}}
}

@objc protocol StorageProtocol {
  @objcDirect var requirement: Int { get }
  // expected-error@-1 {{'@objcDirect' cannot be applied to protocol requirements}}

  @objcDirect subscript(i: Int) -> Int { get }
  // expected-error@-1 {{'@objcDirect' cannot be applied to protocol requirements}}
}

class GenericStorageClass<T>: NSObject {
  @objcDirect var inGeneric: Int = 0
  // expected-error@-1 {{'@objcDirect' cannot be applied to members of 'GenericStorageClass', which is not exposed to Objective-C}}
}

struct StorageStruct {
  @objcDirect var structProperty: Int = 0
  // expected-error@-1 {{'@objcDirect' can only be applied to members of a class declared at file scope}}

  @objcDirect subscript(i: Int) -> Int { i }
  // expected-error@-1 {{'@objcDirect' can only be applied to members of a class declared at file scope}}
}

@objcDirect var globalProperty: Int = 0
// expected-error@-1 {{'@objcDirect' can only be applied to members of a class declared at file scope}}

@objc protocol HasCount {
  var count: Int { get } // expected-note {{requirement 'count' declared here}}
  var size: Int { get }
}

class DirectCount: NSObject, HasCount {
  @objcDirect var count: Int = 0
  // expected-error@-1 {{cannot satisfy a requirement of '@objc' protocol 'HasCount'}}

  var size: Int = 0
}

// MARK: - Class names

class Outer: NSObject {
  @objc(OuterNested) class Nested: NSObject {
    @objcDirect func f() {}
    // expected-error@-1 {{'@objcDirect' can only be applied to members of a class declared at file scope}}
  }
}

extension Outer.Nested {
  @objcDirect func g() {}
  // expected-error@-1 {{'@objcDirect' can only be applied to members of a class declared at file scope}}
}

func makeLocal() {
  class Local: NSObject {
    @objcDirect func f() {}
    // expected-error@-1 {{'@objcDirect' can only be applied to members of a class declared at file scope}}
  }
}

// A private class caps its members' scope.
private class PrivateHelper: NSObject {
  @objcDirect func run() {}
  // expected-error@-1 {{an '@objcDirect' instance method must be visible to the whole module}}
}

fileprivate class FilePrivateHelper: NSObject {
  @objcDirect func run() {}
  // expected-error@-1 {{an '@objcDirect' instance method must be visible to the whole module}}
}

class TopLevel: NSObject {
  @objcDirect func f() {}
}

// MARK: - Enclosing classes

class PureSwift {
  @objcDirect func f() {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to members of 'PureSwift', which is not exposed to Objective-C}}
}

class Generic<T>: NSObject {}
class GenericAncestry: Generic<Int> {
  @objcDirect func f() {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to members of 'GenericAncestry', which is not exposed to Objective-C}}
}

open class OpenClass: NSObject {
  @objcDirect open func f() {}
  // expected-error@-1 {{'@objcDirect' declarations are implicitly 'final'; use 'public' instead of 'open'}} {{15-19=public}}
  @objcDirect open var p: Int { 0 }
  // expected-error@-1 {{'@objcDirect' declarations are implicitly 'final'; use 'public' instead of 'open'}} {{15-19=public}}
  @objcDirect public func publicIsFine() {}
}

// MARK: - Selectors

class ProtocolSelectors: NSObject {
  @objc(hash) @objcDirect final func myHash() -> Int { 0 }
  // expected-error@-1 {{instance method 'myHash()' cannot be '@objcDirect': its Objective-C selector 'hash' is a requirement of protocol 'NSObjectProtocol', and a method that implements a protocol requirement cannot be direct}}

  @objc(description) @objcDirect var desc: String { "" }
  // expected-error@-1 {{property 'desc' cannot be '@objcDirect': its Objective-C selector 'description' is a requirement of protocol 'NSObjectProtocol', and a method that implements a protocol requirement cannot be direct}}

  @objc(isEqual:) @objcDirect final func same(_ other: Any?) -> Bool { false }
  // expected-error@-1 {{instance method 'same' cannot be '@objcDirect': its Objective-C selector 'isEqual:' is a requirement of protocol 'NSObjectProtocol', and a method that implements a protocol requirement cannot be direct}}

  @objc(hashValueForDisplay) @objcDirect final func displayHash() -> Int { 0 }
}

class Subscripts: NSObject {
  @objcDirect subscript(a: Int, b: Int) -> Int { 0 }
  // expected-error@-1 {{an '@objcDirect' subscript must take exactly one index}}

  @objcDirect subscript(a: Int) -> Int { 0 }
}
