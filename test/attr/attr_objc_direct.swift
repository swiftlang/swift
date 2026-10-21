// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ObjCDirect

import Foundation

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
  // expected-error@-2 {{'private' or 'fileprivate' methods cannot be '@objcDirect'}}
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

// Properties, subscripts and accessors are not in it.
class DeinitClass: NSObject {
  @objcDirect deinit {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to 'deinit'}}
}

class PropertyClass: NSObject {
  @objcDirect var x: Int = 0
  // expected-error@-1 {{cannot be applied to this declaration}}

  var y: Int {
    @objcDirect get { return 0 }
    // expected-error@-1 {{cannot be applied to this declaration}}
    set {}
  }

  @objcDirect subscript(i: Int) -> Int { return i }
  // expected-error@-1 {{cannot be applied to this declaration}}
}

extension ObjCDirectClass {
  @objcDirect
  public final func inExtension() { return }
}

extension ObjCDirectClass {
  @objc
  public func overridable() { return }
}

class SubClass: ObjCDirectClass {
  @objcDirect
  public final func notAnOverride() { return }

  @objcDirect
  public final func asyncMethod() async -> Int { return 1 }
  // expected-error@-2 {{'@objcDirect' is not supported with 'async' methods}}

  @objcDirect
  public override final func overridable() { return }
  // expected-error@-2 {{'@objcDirect' methods cannot override superclass methods}}

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
