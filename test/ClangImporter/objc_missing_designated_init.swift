// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s -swift-version 4 -verify

// REQUIRES: objc_interop

import UnimportableMembers

class IncompleteInitSubclassImplicit : IncompleteDesignatedInitializers {
  var myOneNewMember = 1
}

class IncompleteInitSubclass : IncompleteDesignatedInitializers {
  override init(first: Int) {}
  override init(second: Int) {}
}

class IncompleteConvenienceInitSubclass : IncompleteConvenienceInitializers {}

class IncompleteUnknownInitSubclass : IncompleteUnknownInitializers {}

func testBaseClassesBehaveAsExpected() {
  _ = IncompleteDesignatedInitializers(first: 0) // okay
  _ = IncompleteDesignatedInitializers(second: 0) // okay
  _ = IncompleteDesignatedInitializers(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteDesignatedInitializers(conveniently: 0) // okay
  _ = IncompleteDesignatedInitializers(category: 0) // okay

  _ = IncompleteConvenienceInitializers(first: 0) // okay
  _ = IncompleteConvenienceInitializers(second: 0) // okay
  _ = IncompleteConvenienceInitializers(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteConvenienceInitializers(conveniently: 0) // okay
  _ = IncompleteConvenienceInitializers(category: 0) // okay

  _ = IncompleteUnknownInitializers(first: 0) // okay
  _ = IncompleteUnknownInitializers(second: 0) // okay
  _ = IncompleteUnknownInitializers(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteUnknownInitializers(conveniently: 0) // okay
  _ = IncompleteUnknownInitializers(category: 0) // okay
}

func testSubclasses() {
  _ = IncompleteInitSubclass(first: 0) // okay
  _ = IncompleteInitSubclass(second: 0) // okay
  _ = IncompleteInitSubclass(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteInitSubclass(conveniently: 0) // expected-error {{argument labels '(conveniently:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteInitSubclass(category: 0) // expected-error {{argument labels '(category:)' do not match any available overloads}} expected-note {{overloads}}

  _ = IncompleteInitSubclassImplicit(first: 0) // okay
  _ = IncompleteInitSubclassImplicit(second: 0) // okay
  _ = IncompleteInitSubclassImplicit(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteInitSubclassImplicit(conveniently: 0) // expected-error {{argument labels '(conveniently:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteInitSubclassImplicit(category: 0) // expected-error {{argument labels '(category:)' do not match any available overloads}} expected-note {{overloads}}

  _ = IncompleteConvenienceInitSubclass(first: 0) // okay
  _ = IncompleteConvenienceInitSubclass(second: 0) // okay
  _ = IncompleteConvenienceInitSubclass(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteConvenienceInitSubclass(conveniently: 0) // okay
  _ = IncompleteConvenienceInitSubclass(category: 0) // okay

  _ = IncompleteUnknownInitSubclass(first: 0) // okay
  _ = IncompleteUnknownInitSubclass(second: 0) // okay
  _ = IncompleteUnknownInitSubclass(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteUnknownInitSubclass(conveniently: 0) // okay

  // FIXME: This initializer isn't being inherited for some reason, unrelated
  // to the non-importable -initMissing:.
  // https://bugs.swift.org/browse/SR-4566
  _ = IncompleteUnknownInitSubclass(category: 0) // expected-error {{argument labels '(category:)' do not match any available overloads}} expected-note {{overloads}}
}
