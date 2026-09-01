// REQUIRES: objc_interop

// RUN: %target-typecheck-verify-swift -Xcc -fobjc-direct-precondition-thunk

import Foundation

class ObjCDirectClass: NSObject {
  @objc @objcDirect
  public final func direct() { return }

  @objc @objcDirect
  public final func withParams(x: Int, y: String) { return }

  @objc(RenamedMethod) @objcDirect
  public final func renamed() { return }

  @objc @objcDirect
  public static func staticMethod() -> Int { return 42 }

  @objc @objcDirect
  public func nonFinal() { return }
  // expected-error@-2 {{'@objcDirect' methods must be 'final' to prevent overriding}}

  @objc @objcDirect
  public final func variadic(format: String, args: CVarArg...) { return }
  // expected-error@-1 {{instance method cannot be marked '@objc' because it has a variadic parameter}}

  @objc @objcDirect
  private final func privateMethod() { return }
  // expected-error@-2 {{'private' or 'fileprivate' methods cannot be '@objcDirect'}}
}

// All members of a `final class` are implicitly final, so no explicit `final`
// is needed.
final class FinalClass: NSObject {
  @objc @objcDirect
  public func implicitlyFinal() { return }

  @objcDirect
  public func implicitlyFinalNoObjC() { return }
}

// @objcDirect implies @objc: a representable signature is accepted with no
// explicit @objc.
class ImpliedObjC: NSObject {
  @objcDirect
  final func implied() -> Int { return 1 }

  @objcDirect
  final func impliedWithParams(x: Int) -> String { return "" }

  // The implicit @objc is validated rather than silently added, so a signature
  // that is not representable in Objective-C is still rejected -- by the
  // standard @objc diagnostics.
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
  @objc @objcDirect
  init(ok value: Int) { super.init() }

  @objc @objcDirect
  required init(bad value: String) { super.init() }
  // expected-error@-2 {{'@objcDirect' cannot be applied to 'required' initializers}}
}

class DeinitClass: NSObject {
  @objcDirect deinit {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to 'deinit'}}
}

class PropertyClass: NSObject {
  @objcDirect var x: Int = 0
  // expected-error@-1 {{cannot be applied to this declaration}}
}

extension ObjCDirectClass {
  @objc @objcDirect
  public final func inExtension() { return }
}

extension ObjCDirectClass {
  @objc
  public func overridable() { return }
}

class SubClass: ObjCDirectClass {
  @objcDirect
  public final func notAnOverride() { return }

  @objc @objcDirect
  public final func asyncMethod() async -> Int { return 1 }
  // expected-error@-2 {{'@objcDirect' is not supported with 'async' methods}}

  @objc @objcDirect
  public override final func overridable() { return }
  // expected-error@-2 {{'@objcDirect' methods cannot override superclass methods}}
}

@objc
protocol DirectProtocol {
  @objc @objcDirect
  func requirement()
  // expected-error@-2 {{'@objcDirect' cannot be applied to protocol requirements}}
}

// A generic class is never printed to the generated ObjC header, so a direct
// symbol on one could never be referenced from Objective-C -- even though the
// standard @objc check permits the member.
class GenericClass<T>: NSObject {
  @objc @objcDirect
  public final func inGeneric() { return }
  // expected-error@-2 {{'@objcDirect' cannot be applied to members of a generic class}}
}

// Only class members: a global function, or a struct or enum member, has no
// printed @interface name to mangle against.
@objcDirect
public func globalFunction() {}
// expected-error@-2 {{'@objcDirect' can only be applied to members of a class}}

struct SomeStruct {
  @objcDirect func structMethod() {}
  // expected-error@-1 {{'@objcDirect' can only be applied to members of a class}}
}

enum SomeEnum {
  @objcDirect static func enumMethod() {}
  // expected-error@-1 {{'@objcDirect' can only be applied to members of a class}}
}
