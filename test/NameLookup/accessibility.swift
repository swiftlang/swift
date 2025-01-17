// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/has_accessibility.swift -D DEFINE_VAR_FOR_SCOPED_IMPORT -enable-testing
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -I %t -sdk "" -enable-access-control -verify -verify-ignore-unknown
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -I %t -sdk "" -disable-access-control -D ACCESS_DISABLED
// RUN: not %target-swift-frontend -typecheck -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -I %t -sdk "" -D TESTABLE 2>&1 | %FileCheck -check-prefix=TESTABLE %s

#if TESTABLE
@testable import has_accessibility
#else
import has_accessibility
#endif

// This deliberately has the wrong import kind.
import var has_accessibility.zz // expected-error {{variable 'zz' does not exist in module 'has_accessibility'}}

func markUsed<T>(_ t: T) {}

markUsed(has_accessibility.x)
markUsed(has_accessibility.y) // expected-error {{module 'has_accessibility' has no member named 'y'}}
markUsed(has_accessibility.z) // expected-error {{module 'has_accessibility' has no member named 'z'}}
// TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
// TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
// TESTABLE: :[[@LINE-3]]:10: error: module 'has_accessibility' has no member named 'z'

markUsed(accessibility.a)
markUsed(accessibility.b)
markUsed(accessibility.c) // expected-error {{'c' is inaccessible due to 'private' protection level}}

markUsed(x)
markUsed(y) // expected-error {{cannot find 'y' in scope}}
markUsed(z) // expected-error {{cannot find 'z' in scope}}
// TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
// TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
// TESTABLE: :[[@LINE-3]]:10: error: cannot find 'z' in scope

markUsed(a)
markUsed(b)
markUsed(c) // expected-error {{cannot find 'c' in scope}}

Foo.x()
Foo.y() // expected-error {{'y' is inaccessible due to 'internal' protection level}}
Foo.z() // expected-error {{'z' is inaccessible due to 'private' protection level}}
// TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
// TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
// TESTABLE: :[[@LINE-3]]:{{[^:]+}}: error: 'z' is inaccessible due to 'private' protection level
Foo.a()
Foo.b()
Foo.c() // expected-error {{'c' is inaccessible due to 'private' protection level}}

_ = Foo() // expected-error {{'Foo' initializer is inaccessible due to 'internal' protection level}}

// <rdar://problem/27982012> QoI: Poor diagnostic for inaccessible initializer
struct rdar27982012 {
  var x: Int
  private init(_ x: Int) { self.x = x } // expected-note {{'init(_:)' declared here}}
}

_ = { rdar27982012($0.0) }((1, 2)) // expected-error {{initializer is inaccessible due to 'private' protection level}}

// TESTABLE-NOT: :[[@LINE-1]]:{{[^:]+}}:
_ = PrivateInit() // expected-error {{'PrivateInit' initializer is inaccessible due to 'private' protection level}}
// TESTABLE: :[[@LINE-1]]:{{[^:]+}}: error: 'PrivateInit' initializer is inaccessible due to 'private' protection level

var s = StructWithPrivateSetter()
// expected-note@-1 3{{did you mean 's'?}}
s.x = 42 // expected-error {{cannot assign to property: 'x' setter is inaccessible}}

class Sub : Base {
  func test() {
    value = 4 // expected-error {{cannot assign to property: 'value' setter is inaccessible}}
    self.value = 4 // expected-error {{cannot assign to property: 'value' setter is inaccessible}}
    super.value = 4 // expected-error {{cannot assign to property: 'value' setter is inaccessible}}
    // TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
    // TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
    // TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:

    method() // expected-error {{'method' is inaccessible due to 'internal' protection level}}
    self.method() // expected-error {{'method' is inaccessible due to 'internal' protection level}}
    super.method() // expected-error {{'method' is inaccessible due to 'internal' protection level}}
    // TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
    // TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
    // TESTABLE-NOT: :[[@LINE-3]]:{{[^:]+}}:
  }
}

class ObservingOverrider : Base {
  override var value: Int { // expected-error {{cannot observe read-only property 'value'; it can't change}}
    willSet { markUsed(newValue) }
  }
}

class ReplacingOverrider : Base {
  override var value: Int {
    get { return super.value }
    set { super.value = newValue } // expected-error {{cannot assign to property: 'value' setter is inaccessible}}
  }
}

protocol MethodProto {
  func method() // expected-note * {{protocol requires function 'method()' with type '() -> ()'}}
}

extension OriginallyEmpty : MethodProto {}
#if !ACCESS_DISABLED
extension HiddenMethod : MethodProto {} // expected-error {{type 'HiddenMethod' does not conform to protocol 'MethodProto'}} expected-note {{add stubs for conformance}}
// TESTABLE-NOT: :[[@LINE-1]]:{{[^:]+}}:

extension Foo : MethodProto {} // expected-error {{type 'Foo' does not conform to protocol 'MethodProto'}} expected-note {{add stubs for conformance}}
#endif


protocol TypeProto {
  associatedtype TheType // expected-note * {{protocol requires nested type 'TheType'}}
}

extension OriginallyEmpty {}
#if !ACCESS_DISABLED
extension HiddenType : TypeProto {} // expected-error {{type 'HiddenType' does not conform to protocol 'TypeProto'}} expected-note {{add stubs for conformance}}
// TESTABLE-NOT: :[[@LINE-1]]:{{[^:]+}}:

extension Foo : TypeProto {} // expected-error {{type 'Foo' does not conform to protocol 'TypeProto'}} expected-note {{add stubs for conformance}}
#endif


#if !ACCESS_DISABLED
private func privateInBothFiles() {} // no-warning
private func privateInPrimaryFile() {} // expected-error {{invalid redeclaration}}
func privateInOtherFile() {}
#endif


#if !ACCESS_DISABLED
struct ConformerByTypeAlias : TypeProto {
  private typealias TheType = Int // expected-error {{type alias 'TheType' must be declared internal because it matches a requirement in internal protocol 'TypeProto'}} {{none}}
  // expected-note@-1 {{mark the type alias as 'internal' to satisfy the requirement}} {{3-10=internal}}
}

struct ConformerByLocalType : TypeProto {
  private struct TheType {} // expected-error {{struct 'TheType' must be declared internal because it matches a requirement in internal protocol 'TypeProto'}} {{none}}
  // expected-note@-1 {{mark the struct as 'internal' to satisfy the requirement}} {{3-10=internal}}
}

private struct PrivateConformerByLocalType : TypeProto {
  struct TheType {} // okay
}

private struct PrivateConformerByLocalTypeBad : TypeProto {
  private struct TheType {} // expected-error {{struct 'TheType' must be as accessible as its enclosing type because it matches a requirement in protocol 'TypeProto'}} {{none}}
  // expected-note@-1 {{mark the struct as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
}
#endif

public protocol Fooable {
  func foo() // expected-note * {{protocol requires function 'foo()'}}
}

#if !ACCESS_DISABLED
internal struct FooImpl: Fooable, HasDefaultImplementation {} // expected-error {{type 'FooImpl' does not conform to protocol 'Fooable'}} expected-note {{add stubs for conformance}}
public struct PublicFooImpl: Fooable, HasDefaultImplementation {} // expected-error {{type 'PublicFooImpl' does not conform to protocol 'Fooable'}} expected-note {{add stubs for conformance}}
// TESTABLE-NOT: method 'foo()'

internal class TestableSub: InternalBase {} // expected-error {{cannot find type 'InternalBase' in scope}}
public class TestablePublicSub: InternalBase {} // expected-error {{cannot find type 'InternalBase' in scope}}
// TESTABLE-NOT: cannot find type 'InternalBase' in scope
#endif

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: 'y' declared here
// <unknown>:0: error: unexpected note produced: 'z' declared here
// <unknown>:0: error: unexpected note produced: 'init()' declared here
// <unknown>:0: error: unexpected note produced: 'method()' declared here
// <unknown>:0: error: unexpected note produced: 'method' declared here
// <unknown>:0: error: unexpected note produced: 'method' declared here

class AccessMemberOfInternalProtocol : ImplementsInternalProtocol {
  func testProperty() { let _ = i } // expected-error {{'i' is inaccessible due to 'internal' protection level}}
}
