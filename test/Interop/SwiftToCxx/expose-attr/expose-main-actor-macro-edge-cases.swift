// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Edge -target %target-swift-5.1-abi-triple -cxx-interoperability-mode=default -plugin-path %swift-plugin-dir -parse-as-library -swift-version 6 -strict-memory-safety -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=has-expose-attr -emit-module -o %t -verify -emit-clang-header-path %t/edge.h
// RUN: %FileCheck %s < %t/edge.h
// RUN: %FileCheck %s --check-prefix=NONCOPYABLE < %t/edge.h

// Declarations whose thunks need special care. They compile in the Swift 6
// language mode, with region isolation checked on SIL.

// REQUIRES: swift_swift_parser
// REQUIRES: concurrency
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// REQUIRES: swift_feature_GenerateBindingsForNoncopyableTypesInCXX

public protocol P {}
extension Int: P {}
public struct E: Error {}

@_expose(Cxx)
public struct Edge {
  public var x = 0

  // CHECK: swift::ThrowingResult<swift::Int> typedThrows() const SWIFT_SYMBOL("{{.*}}_cxx_typedThrows{{.*}}")
  @_ExposeMainActorToCxx
  @MainActor public func typedThrows() throws(E) -> Int { x }

  // CHECK: swift::ThrowingResult<swift::Int> untypedThrows() const SWIFT_SYMBOL("{{.*}}_cxx_untypedThrows{{.*}}")
  @_ExposeMainActorToCxx
  @MainActor public func untypedThrows() throws -> Int { x }

  // CHECK: swift::Int discardable() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_discardable{{.*}}")
  @_ExposeMainActorToCxx
  @discardableResult
  @MainActor public func discardable() -> Int { x }

  // Opaque results are not representable in C++, but the thunk compiles.
  @_ExposeMainActorToCxx
  @MainActor public func opaque() -> some P { x }

  // CHECK: static SWIFT_INLINE_THUNK swift::Optional<Edge> init(swift::Int failable) noexcept SWIFT_SYMBOL("{{.*}}_cxx_init{{.*}}")
  @_ExposeMainActorToCxx
  @MainActor public init?(failable: Int) { return nil }

  // CHECK: swift::Int getObserved() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_observed{{.*}}")
  // CHECK: void setObserved(swift::Int newValue) noexcept SWIFT_SYMBOL("{{.*}}_cxx_observed{{.*}}")
  @_ExposeMainActorToCxx
  @MainActor public var observed: Int = 0 { didSet { x = observed } }

  // CHECK: swift::Int getNonmutatingSetter() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_nonmutatingSetter{{.*}}")
  // CHECK: void setNonmutatingSetter(swift::Int newValue) noexcept SWIFT_SYMBOL("{{.*}}_cxx_nonmutatingSetter{{.*}}")
  @_ExposeMainActorToCxx
  @MainActor public var nonmutatingSetter: Int {
    get { 0 }
    nonmutating set {}
  }

  // A mutating getter writes `self` back. (PrintAsClang does not print the
  // getter as non-const yet, so this is not checked in the header.)
  @_ExposeMainActorToCxx
  @MainActor public var mutatingGetter: Int {
    mutating get { x += 1; return x }
  }
}

// The member macro skips what the peer macro would diagnose.
@_expose(Cxx)
@MainActor @_ExposeMainActorMembersToCxx
public struct Skipped {
  public var x = 0
  public static subscript(i: Int) -> Int { i }
  public func variadic(_ xs: Int...) {}
  public func autoclosure(_ x: @autoclosure () -> Int) {}
  public static func +(lhs: Skipped, rhs: Skipped) -> Skipped { lhs }
  @_ExposeMainActorToCxx("renamed")
  public func alreadyExposed() {}
}

// CHECK: class SWIFT_SYMBOL("{{.*}}") Skipped final {
// CHECK-NOT: variadic
// CHECK-NOT: autoclosure
// CHECK-NOT: operator+
// CHECK: void renamed() const noexcept SWIFT_SYMBOL("{{.*}}_cxx_alreadyExposed{{.*}}")
// CHECK-NOT: alreadyExposed()
// CHECK: };

// `@discardableResult` is copied onto the thunk.
// CHECK: SWIFT_INLINE_THUNK swift::Int Edge::discardable() const noexcept{{( SWIFT_AVAILABILITY\([a-z]+,introduced=27\))+}} {

// Noncopyable values are borrowed and mutated in place, and moved when
// consumed.
@_expose(Cxx)
@MainActor @_ExposeMainActorMembersToCxx
public struct Token: ~Copyable {
  public var id: Int
  public init(id: Int) { self.id = id }
  public func compare(_ other: borrowing Token) -> Bool { id == other.id }
  public mutating func bump() { id += 1 }
  public func replace(_ other: inout Token) { other.id = id }
  public consuming func finish() -> Int { id }
}

// NONCOPYABLE: class SWIFT_SYMBOL("{{.*}}") Token final {
// NONCOPYABLE: static SWIFT_INLINE_THUNK Token init(swift::Int id) noexcept SWIFT_SYMBOL("{{.*}}_cxx_init{{.*}}")
// NONCOPYABLE: bool compare(const Token& other) const noexcept SWIFT_SYMBOL("{{.*}}_cxx_compare{{.*}}")
// NONCOPYABLE: void bump() noexcept SWIFT_SYMBOL("{{.*}}_cxx_bump{{.*}}")
// NONCOPYABLE: void replace(Token& other) const noexcept SWIFT_SYMBOL("{{.*}}_cxx_replace{{.*}}")
// PrintAsClang does not expose consuming methods of noncopyable types yet, but
// the thunk compiles.
// NONCOPYABLE-NOT: finish()
// NONCOPYABLE: };
