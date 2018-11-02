// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -I %S/Inputs/custom-modules %s -swift-version 4 -verify

import UnimportableMembers
import UnimportableMembersUser

class IncompleteInitSubclassImplicit : IncompleteDesignatedInitializers {
  var myOneNewMember = 1
}

class IncompleteInitSubclass : IncompleteDesignatedInitializers {
  override init(first: Int) {}
  override init(second: Int) {}
}

class IncompleteConvenienceInitSubclass : IncompleteConvenienceInitializers {}

class IncompleteUnknownInitSubclass : IncompleteUnknownInitializers {}

class IncompleteInitCategorySubclassImplicit : IncompleteDesignatedInitializersWithCategory {}

class IncompleteInitCategorySubclass : IncompleteDesignatedInitializersWithCategory {
  override init(first: Int) {}
  override init(second: Int) {}
}

class DesignatedInitializerInAnotherModuleSubclass : DesignatedInitializerInAnotherModule {}


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

  _ = IncompleteDesignatedInitializersWithCategory(first: 0) // okay
  _ = IncompleteDesignatedInitializersWithCategory(second: 0) // okay
  _ = IncompleteDesignatedInitializersWithCategory(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteDesignatedInitializersWithCategory(conveniently: 0) // okay
  _ = IncompleteDesignatedInitializersWithCategory(category: 0) // okay

  _ = DesignatedInitializerInAnotherModule(first: 0) // okay
  _ = DesignatedInitializerInAnotherModule(second: 0) // okay
  _ = DesignatedInitializerInAnotherModule(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = DesignatedInitializerInAnotherModule(conveniently: 0) // okay
  _ = DesignatedInitializerInAnotherModule(category: 0) // okay
  _ = DesignatedInitializerInAnotherModule(fromOtherModule: 0) // okay
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
  _ = IncompleteUnknownInitSubclass(category: 0) // okay

  _ = IncompleteInitCategorySubclass(first: 0) // okay
  _ = IncompleteInitCategorySubclass(second: 0) // okay
  _ = IncompleteInitCategorySubclass(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteInitCategorySubclass(conveniently: 0) // expected-error {{argument labels '(conveniently:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteInitCategorySubclass(category: 0) // expected-error {{argument labels '(category:)' do not match any available overloads}} expected-note {{overloads}}

  _ = IncompleteInitCategorySubclassImplicit(first: 0) // okay
  _ = IncompleteInitCategorySubclassImplicit(second: 0) // okay
  _ = IncompleteInitCategorySubclassImplicit(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteInitCategorySubclassImplicit(conveniently: 0) // expected-error {{argument labels '(conveniently:)' do not match any available overloads}} expected-note {{overloads}}
  _ = IncompleteInitCategorySubclassImplicit(category: 0) // expected-error {{argument labels '(category:)' do not match any available overloads}} expected-note {{overloads}}

  _ = DesignatedInitializerInAnotherModuleSubclass(first: 0) // okay
  _ = DesignatedInitializerInAnotherModuleSubclass(second: 0) // okay
  _ = DesignatedInitializerInAnotherModuleSubclass(missing: 0) // expected-error {{argument labels '(missing:)' do not match any available overloads}} expected-note {{overloads}}
  _ = DesignatedInitializerInAnotherModuleSubclass(conveniently: 0) // expected-error {{argument labels '(conveniently:)' do not match any available overloads}} expected-note {{overloads}}
  _ = DesignatedInitializerInAnotherModuleSubclass(category: 0) // expected-error {{argument labels '(category:)' do not match any available overloads}} expected-note {{overloads}}
  _ = DesignatedInitializerInAnotherModuleSubclass(fromOtherModule: 0) // okay
}
