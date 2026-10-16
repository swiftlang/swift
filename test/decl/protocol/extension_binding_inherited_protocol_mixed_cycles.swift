// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/First.swift
// RUN: %target-swift-frontend -typecheck -verify %t/Last.swift
// RUN: %target-swift-frontend -typecheck -verify %t/DirectFirst.swift
// RUN: %target-swift-frontend -typecheck -verify %t/DirectLast.swift
// RUN: %target-swift-frontend -typecheck -verify %t/AliasFirst.swift
// RUN: %target-swift-frontend -typecheck -verify %t/AliasLast.swift
// RUN: %target-swift-frontend -typecheck -verify %t/WhereFirst.swift
// RUN: %target-swift-frontend -typecheck -verify %t/WhereLast.swift
// RUN: %target-swift-frontend -typecheck -verify %t/AnyObject.swift

// Preserve the protocols found in a cyclic composition while resolving a
// different entry after more extensions are bound. Cover both entry orders.

//--- First.swift
protocol Top {}
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
protocol N: A.OwnAlias & Top, A.P { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { protocol P {} }

func takesTop(_: any Top) {}
func takesP(_: any A.P) {}
func check<T: N>(_ value: T) { takesTop(value); takesP(value) }

//--- Last.swift
protocol Top {}
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
protocol N: A.P, A.OwnAlias & Top { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { protocol P {} }

func takesTop(_: any Top) {}
func takesP(_: any A.P) {}
func check<T: N>(_ value: T) { takesTop(value); takesP(value) }

// Resolve a late component of the same composition, including through a
// typealias or a Self constraint. An unrelated missing type must not cause
// the cached inherited protocols to disagree with the requirement signature.

//--- DirectFirst.swift
protocol Top {}
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
protocol N: A.OwnAlias & A.P, Missing { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}} expected-error {{cannot find type 'Missing' in scope}} expected-warning {{protocol 'N' should be declared to refine 'Top' due to a same-type constraint on 'Self'}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { protocol P {} }

func takesP(_: any A.P) {}
func check<T: N>(_ value: T) { takesP(value) }

//--- DirectLast.swift
protocol Top {}
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
protocol N: Missing, A.OwnAlias & A.P { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}} expected-error {{cannot find type 'Missing' in scope}} expected-warning {{protocol 'N' should be declared to refine 'Top' due to a same-type constraint on 'Self'}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { protocol P {} }

func takesP(_: any A.P) {}
func check<T: N>(_ value: T) { takesP(value) }

//--- AliasFirst.swift
protocol Top {}
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
typealias Mixed = A.OwnAlias & A.P
protocol N: Mixed, Missing { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}} expected-error {{cannot find type 'Missing' in scope}} expected-warning {{protocol 'N' should be declared to refine 'Top' due to a same-type constraint on 'Self'}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { protocol P {} }

func takesP(_: any A.P) {}
func check<T: N>(_ value: T) { takesP(value) }

//--- AliasLast.swift
protocol Top {}
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
typealias Mixed = A.OwnAlias & A.P
protocol N: Missing, Mixed { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}} expected-error {{cannot find type 'Missing' in scope}} expected-warning {{protocol 'N' should be declared to refine 'Top' due to a same-type constraint on 'Self'}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { protocol P {} }

func takesP(_: any A.P) {}
func check<T: N>(_ value: T) { takesP(value) }

//--- WhereFirst.swift
protocol Top {}
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
protocol N where Self: A.OwnAlias & A.P, Self: Missing { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}} expected-error {{cannot find type 'Missing' in scope}} expected-warning {{protocol 'N' should be declared to refine 'Top' due to a same-type constraint on 'Self'}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { protocol P {} }

func takesP(_: any A.P) {}
func check<T: N>(_ value: T) { takesP(value) }

//--- WhereLast.swift
protocol Top {}
enum A {}
extension A { typealias OwnAlias = N.Own } // expected-error {{circular reference}} expected-note {{through reference here}}
protocol N where Self: Missing, Self: A.OwnAlias & A.P { // expected-error {{'N' inherits from itself}} expected-note {{through reference here}} expected-error {{cannot find type 'Missing' in scope}} expected-warning {{protocol 'N' should be declared to refine 'Top' due to a same-type constraint on 'Self'}}
  typealias T = Int
  typealias Own = Top
}
extension N.T {}
extension A { protocol P {} }

func takesP(_: any A.P) {}
func check<T: N>(_ value: T) { takesP(value) }

//--- AnyObject.swift
protocol P {}
typealias AnyObject = Swift.AnyObject & P
protocol N: AnyObject {}
func takesP(_: any P) {}
func check<T: N>(_ value: T) { takesP(value) }
