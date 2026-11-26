// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name MainActorMacro -target %target-swift-5.1-abi-triple -plugin-path %swift-plugin-dir -parse-as-library -strict-memory-safety -clang-header-expose-decls=has-expose-attr -emit-module -emit-module-doc -o %t -verify -emit-clang-header-path %t/macro.h
// RUN: %FileCheck %s --check-prefix=FUNC < %t/macro.h
// RUN: %FileCheck %s --check-prefix=PARTIAL < %t/macro.h
// RUN: %FileCheck %s --check-prefix=BASE < %t/macro.h
// RUN: %FileCheck %s --check-prefix=TYPES < %t/macro.h
// RUN: %check-interop-cxx-header-in-clang(%t/macro.h -Wno-error=unused-function)

// The thunks also compile in the Swift 6 language mode, including those passing
// non-Sendable values (region isolation is checked on SIL, so -typecheck is not
// enough).
// RUN: %target-swift-frontend %s -module-name MainActorMacro -target %target-swift-5.1-abi-triple -cxx-interoperability-mode=default -plugin-path %swift-plugin-dir -parse-as-library -swift-version 6 -strict-memory-safety -emit-sil -o /dev/null -verify

// The thunks round-trip through module interfaces.
// RUN: %target-swift-frontend %s -module-name MainActorMacro -target %target-swift-5.1-abi-triple -plugin-path %swift-plugin-dir -parse-as-library -swift-version 5 -enable-library-evolution -typecheck -emit-module-interface-path %t/MainActorMacro.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/MainActorMacro.swiftinterface) -module-name MainActorMacro
// RUN: %FileCheck %s --check-prefix=INTERFACE < %t/MainActorMacro.swiftinterface

// REQUIRES: swift_swift_parser
// REQUIRES: concurrency

// The macros are declared in the Cxx module, which is imported implicitly when
// C++ interoperability is enabled (but not with library evolution).
import Cxx

@_expose(Cxx)
public class NonSendable {
  public init() {}
}

// Free functions.

@_ExposeMainActorToCxx
@MainActor public func mainActorFunc(_ x: Int) -> Int { x + 1 }

@_ExposeMainActorToCxx("renamedFunc")
@MainActor public func mainActorFuncRenamed(label x: Int, _ y: inout Int) { y = x }

@_ExposeMainActorToCxx
@MainActor public func genericFunc<T>(_ x: T) -> T { x }

@_ExposeMainActorToCxx
@MainActor public func roundTrip(_ x: NonSendable) -> NonSendable { x }

// FUNC: SWIFT_INLINE_THUNK T_0_0 genericFunc(const T_0_0& x) noexcept SWIFT_SYMBOL("{{.*}}_cxx_genericFunc{{.*}}")
// FUNC: SWIFT_INLINE_THUNK swift::Int mainActorFunc(swift::Int x) noexcept SWIFT_SYMBOL("{{.*}}_cxx_main{{.*}}")
// FUNC: SWIFT_INLINE_THUNK void renamedFunc(swift::Int x, swift::Int & y) noexcept SWIFT_SYMBOL("{{.*}}_cxx_main{{.*}}")
// FUNC: SWIFT_INLINE_THUNK NonSendable roundTrip(const NonSendable& x) noexcept SWIFT_SYMBOL("{{.*}}_cxx_roundTrip{{.*}}")

// Individual members of a nonisolated type.

@_expose(Cxx)
public struct PartiallyIsolatedStruct {
  public var x: Int

  @_ExposeMainActorToCxx
  @MainActor public init(x: Int) { self.x = x }

  @_ExposeMainActorToCxx
  @MainActor public func method() -> Int { x }

  @_ExposeMainActorToCxx
  @MainActor public mutating func mutatingMethod() { x += 1 }

  @_ExposeMainActorToCxx
  @MainActor public static func staticMethod() -> Int { 42 }

  @_ExposeMainActorToCxx
  @MainActor public var computed: Int {
    get { x }
    set { x = newValue }
  }

  @_ExposeMainActorToCxx
  @MainActor public var readOnly: Int { x }

  @_ExposeMainActorToCxx
  @MainActor public static var staticProp: Int = 0

  @_ExposeMainActorToCxx
  @MainActor public subscript(offset: Int) -> Int { x + offset }

  /// Names that need escaping.
  @_ExposeMainActorToCxx
  @MainActor public func `default`(_ `in`: Int, `self`: Int) -> Int { `in` + x }

  @_ExposeMainActorToCxx
  @MainActor public func sendingResult() -> sending NonSendable { NonSendable() }
}

extension PartiallyIsolatedStruct {
  @_ExposeMainActorToCxx
  @MainActor public func extensionMethod() -> Int { x }
}

// PARTIAL: class SWIFT_SYMBOL("{{.*}}") PartiallyIsolatedStruct final {
// PARTIAL: static SWIFT_INLINE_THUNK PartiallyIsolatedStruct init(swift::Int x) noexcept SWIFT_SYMBOL("{{.*}}_cxx_init{{.*}}")
// PARTIAL: SWIFT_INLINE_THUNK swift::Int method() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_method{{.*}}")
// PARTIAL: SWIFT_INLINE_THUNK void mutatingMethod() noexcept SWIFT_SYMBOL("{{.*}}_cxx_mutatingMethod{{.*}}")
// PARTIAL: static SWIFT_INLINE_THUNK swift::Int staticMethod() noexcept SWIFT_SYMBOL("{{.*}}_cxx_staticMethod{{.*}}")
// PARTIAL: SWIFT_INLINE_THUNK swift::Int getComputed() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_computed{{.*}}")
// PARTIAL: SWIFT_INLINE_THUNK void setComputed(swift::Int newValue) noexcept SWIFT_SYMBOL("{{.*}}_cxx_computed{{.*}}")
// PARTIAL: SWIFT_INLINE_THUNK swift::Int getReadOnly() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_readOnly{{.*}}")
// PARTIAL-NOT: setReadOnly
// PARTIAL: static SWIFT_INLINE_THUNK swift::Int getStaticProp() noexcept SWIFT_SYMBOL("{{.*}}_cxx_staticProp{{.*}}")
// PARTIAL: static SWIFT_INLINE_THUNK void setStaticProp(swift::Int newValue) noexcept SWIFT_SYMBOL("{{.*}}_cxx_staticProp{{.*}}")
// PARTIAL: SWIFT_INLINE_THUNK swift::Int operator[](swift::Int offset) const noexcept SWIFT_SYMBOL("{{.*}}_cxx_subscript{{.*}}")
// PARTIAL: /// Names that need escaping.
// PARTIAL-NEXT: SWIFT_INLINE_THUNK swift::Int default_(swift::Int in_, swift::Int _1) const noexcept SWIFT_SYMBOL("{{.*}}_cxx_default{{.*}}")
// PARTIAL: SWIFT_INLINE_THUNK NonSendable sendingResult() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_sendingResult{{.*}}")
// PARTIAL: SWIFT_INLINE_THUNK swift::Int extensionMethod() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_extensionMethod{{.*}}")
// PARTIAL: };

// Overrides are reached through the thunk of the overridden method.

@_expose(Cxx)
open class Base {
  public init() {}

  @_ExposeMainActorToCxx
  @MainActor open func virtualMethod() -> Int { 1 }

  @_ExposeMainActorToCxx
  @MainActor open class func classMethod() -> Int { 1 }
}

@_expose(Cxx)
public class Derived: Base {
  @MainActor public override func virtualMethod() -> Int { 2 }
}

// BASE: class SWIFT_SYMBOL("{{.*}}") Base : public swift::_impl::RefCountedClass {
// BASE: SWIFT_INLINE_THUNK swift::Int virtualMethod() noexcept SWIFT_SYMBOL("{{.*}}_cxx_virtualMethod{{.*}}")
// BASE: static SWIFT_INLINE_THUNK swift::Int classMethod() noexcept SWIFT_SYMBOL("{{.*}}_cxx_classMethod{{.*}}")
// BASE: class SWIFT_SYMBOL("{{.*}}") Derived : public Base {
// BASE-NOT: virtualMethod
// BASE: };

// Main-actor-isolated types, where the members get thunks from
// `@_ExposeMainActorMembersToCxx`.

@_expose(Cxx)
@MainActor @_ExposeMainActorMembersToCxx
public final class MainActorClass {
  public init(value: Int) { self.value = value }
  public var value: Int
  public var object: NonSendable = NonSendable()
  public func method() -> Int { value }
  nonisolated public func nonisolatedMethod() -> Int { 42 }
  private func privateMethod() {}

  // Members C++ cannot represent are skipped.
  public var inferredType = 0
  public func throwingMethod() throws {}
  public func constrainedGeneric<T: Equatable>(_ t: T) {}
}

// Overrides are reached through the thunk of the overridden declaration.
@_expose(Cxx)
@MainActor @_ExposeMainActorMembersToCxx
public class MainActorSubclass: MainActorBase {
  public override func method() -> Int { 2 }
}

@_expose(Cxx)
@MainActor @_ExposeMainActorMembersToCxx
open class MainActorBase {
  public init() {}
  open func method() -> Int { 1 }
}

@_expose(Cxx)
@MainActor @_ExposeMainActorMembersToCxx
public struct MainActorStruct {
  public var x: Int
  public init() { x = 0 }
  public mutating func increment() { x += 1 }
  public subscript(label offset: Int) -> Int { x + offset }
}

@_ExposeMainActorMembersToCxx
extension MainActorStruct {
  public func extensionMethod() -> Int { x }
  public var extensionProperty: Int {
    get { x }
    set { x = newValue }
  }
}


// TYPES: class SWIFT_SYMBOL("{{.*}}") MainActorClass final : public swift::_impl::RefCountedClass {
// TYPES: static SWIFT_INLINE_THUNK MainActorClass init(swift::Int value) noexcept SWIFT_SYMBOL("{{.*}}_cxx_init{{.*}}")
// TYPES: SWIFT_INLINE_THUNK swift::Int getValue() noexcept SWIFT_SYMBOL("{{.*}}_cxx_value{{.*}}")
// TYPES: SWIFT_INLINE_THUNK void setValue(swift::Int newValue) noexcept SWIFT_SYMBOL("{{.*}}_cxx_value{{.*}}")
// TYPES: SWIFT_INLINE_THUNK NonSendable getObject() noexcept SWIFT_SYMBOL("{{.*}}_cxx_object{{.*}}")
// TYPES: SWIFT_INLINE_THUNK void setObject(const NonSendable& newValue) noexcept SWIFT_SYMBOL("{{.*}}_cxx_object{{.*}}")
// TYPES: SWIFT_INLINE_THUNK swift::Int method() noexcept SWIFT_SYMBOL("{{.*}}_cxx_method{{.*}}")
// TYPES: SWIFT_INLINE_THUNK swift::Int nonisolatedMethod() noexcept SWIFT_SYMBOL("{{.*}}C17nonisolatedMethod{{.*}}")
// TYPES-NOT: privateMethod
// TYPES-NOT: inferredType
// TYPES-NOT: throwingMethod
// TYPES-NOT: constrainedGeneric
// TYPES: };

// TYPES: class SWIFT_SYMBOL("{{.*}}") MainActorStruct final {
// TYPES: SWIFT_INLINE_THUNK swift::Int getX() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_x{{.*}}")
// TYPES: SWIFT_INLINE_THUNK void setX(swift::Int newValue) noexcept SWIFT_SYMBOL("{{.*}}_cxx_x{{.*}}")
// TYPES: static SWIFT_INLINE_THUNK MainActorStruct init() noexcept SWIFT_SYMBOL("{{.*}}_cxx_init{{.*}}")
// TYPES: SWIFT_INLINE_THUNK void increment() noexcept SWIFT_SYMBOL("{{.*}}_cxx_increment{{.*}}")
// TYPES: SWIFT_INLINE_THUNK swift::Int operator[](swift::Int offset) const noexcept SWIFT_SYMBOL("{{.*}}_cxx_subscript{{.*}}")
// TYPES: SWIFT_INLINE_THUNK swift::Int extensionMethod() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_extensionMethod{{.*}}")
// TYPES: SWIFT_INLINE_THUNK swift::Int getExtensionProperty() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_extensionProperty{{.*}}")
// TYPES: SWIFT_INLINE_THUNK void setExtensionProperty(swift::Int newValue) noexcept SWIFT_SYMBOL("{{.*}}_cxx_extensionProperty{{.*}}")
// TYPES: };

// INTERFACE: @available(swift, obsoleted: 1, message: "only available in C++")
// INTERFACE-NEXT: @_expose(Cxx, "mainActorFunc")
// INTERFACE-NEXT: nonisolated public func _cxx_mainActorFunc(_ x: {{.*}}Int) -> {{.*}}Int
