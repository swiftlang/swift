// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency

actor SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

class Base {}
class GenericBase<T> {}

class Derived1: @MainActor Base {} // expected-error {{global actor 'MainActor' cannot be applied to type in inheritance clause}} {{18-27=}}
class Derived2: @SomeGlobalActor Base {} // expected-error {{global actor 'SomeGlobalActor' cannot be applied to type in inheritance clause}} {{18-33=}}

protocol SomeProtocol {}
class Derived3: @MainActor SomeProtocol {}
struct SomeStruct: @MainActor SomeProtocol {}
enum SomeEnum: @MainActor SomeProtocol {}

class Derived4: @MainActor Base, @MainActor SomeProtocol {} // expected-error {{global actor 'MainActor' cannot be applied to type in inheritance clause}} {{18-27=}}

typealias BaseAlias = Base
class DerivedFromAlias: @MainActor BaseAlias {} // expected-error {{global actor 'MainActor' cannot be applied to type in inheritance clause}} {{26-35=}}

class DerivedFromGeneric: @MainActor GenericBase<Int> {} // expected-error {{global actor 'MainActor' cannot be applied to type in inheritance clause}} {{28-37=}}

protocol ClassBoundProtocol: AnyObject {}
class DerivedClassBound: @MainActor Base, ClassBoundProtocol {} // expected-error {{global actor 'MainActor' cannot be applied to type in inheritance clause}} {{27-36=}}

typealias ComposedType = Base & SomeProtocol
class DerivedFromComposition: @MainActor ComposedType {} // expected-error {{global actor 'MainActor' cannot be applied to type in inheritance clause}} {{32-41=}}
