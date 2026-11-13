// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/SeparateEntry.swift
// RUN: %target-swift-frontend -typecheck -verify %t/Direct.swift
// RUN: %target-swift-frontend -typecheck -verify %t/Alias.swift
// RUN: %target-swift-frontend -typecheck -verify %t/Where.swift

//--- SeparateEntry.swift
// A cyclic entry must not prevent a different entry from finding a superclass
// after more extensions are bound.
protocol Top {}
class Base {}
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
protocol N: A.OwnAlias & Top, A.BaseAlias { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { typealias BaseAlias = Base }

func takesBase(_: Base) {}
func check<T: N>(_ value: T) { takesBase(value) }

// A noncyclic component of the same composition can supply a superclass.
// Its members must become visible to extension binding, including when the
// composition is reached through a typealias or a Self constraint.

//--- Direct.swift
protocol Top {}
class Base { typealias Member = Int }
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
protocol N: A.OwnAlias & Top & A.BaseAlias { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { typealias BaseAlias = Base }
extension N.Member {}

//--- Alias.swift
protocol Top {}
class Base { typealias Member = Int }
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
typealias Mixed = A.OwnAlias & Top & A.BaseAlias
protocol N: Mixed { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { typealias BaseAlias = Base }
extension N.Member {}

//--- Where.swift
protocol Top {}
class Base { typealias Member = Int }
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
protocol N where Self: A.OwnAlias & Top & A.BaseAlias { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { typealias BaseAlias = Base }
extension N.Member {}
