// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -I %S/Inputs/custom-modules %s -swift-version 4 -verify

import UnimportableMembers
import UnimportableMembersUser

class IncompleteInitSubclassImplicit : IncompleteDesignatedInitializers { // expected-note 6 {{incorrect labels for candidate}}
  var myOneNewMember = 1
}

class IncompleteInitSubclass : IncompleteDesignatedInitializers {
  override init(first: Int) {}  // expected-note 3 {{incorrect labels for candidate}}
  override init(second: Int) {} // expected-note 3 {{incorrect labels for candidate}}
}

class IncompleteConvenienceInitSubclass : IncompleteConvenienceInitializers {} // expected-note 2 {{incorrect labels for candidate}}

class IncompleteUnknownInitSubclass : IncompleteUnknownInitializers {} // expected-note 4 {{incorrect labels for candidate}}

class IncompleteInitCategorySubclassImplicit : IncompleteDesignatedInitializersWithCategory {} // expected-note 6 {{incorrect labels for candidate}}

class IncompleteInitCategorySubclass : IncompleteDesignatedInitializersWithCategory {
  override init(first: Int) {}  // expected-note 3 {{incorrect labels for candidate}}
  override init(second: Int) {} // expected-note 3 {{incorrect labels for candidate}}
}

class DesignatedInitializerInAnotherModuleSubclass : DesignatedInitializerInAnotherModule {} // expected-note 9 {{incorrect labels for candidate}}


func testBaseClassesBehaveAsExpected() {
  _ = IncompleteDesignatedInitializers(first: 0) // okay
  _ = IncompleteDesignatedInitializers(second: 0) // okay
  _ = IncompleteDesignatedInitializers(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteDesignatedInitializers(conveniently: 0) // okay
  _ = IncompleteDesignatedInitializers(category: 0) // okay

  _ = IncompleteConvenienceInitializers(first: 0) // okay
  _ = IncompleteConvenienceInitializers(second: 0) // okay
  _ = IncompleteConvenienceInitializers(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteConvenienceInitializers(conveniently: 0) // okay
  _ = IncompleteConvenienceInitializers(category: 0) // okay

  _ = IncompleteUnknownInitializers(first: 0) // okay
  _ = IncompleteUnknownInitializers(second: 0) // okay
  _ = IncompleteUnknownInitializers(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteUnknownInitializers(conveniently: 0) // okay
  _ = IncompleteUnknownInitializers(category: 0) // okay

  _ = IncompleteDesignatedInitializersWithCategory(first: 0) // okay
  _ = IncompleteDesignatedInitializersWithCategory(second: 0) // okay
  _ = IncompleteDesignatedInitializersWithCategory(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteDesignatedInitializersWithCategory(conveniently: 0) // okay
  _ = IncompleteDesignatedInitializersWithCategory(category: 0) // okay

  _ = DesignatedInitializerInAnotherModule(first: 0) // okay
  _ = DesignatedInitializerInAnotherModule(second: 0) // okay
  _ = DesignatedInitializerInAnotherModule(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = DesignatedInitializerInAnotherModule(conveniently: 0) // okay
  _ = DesignatedInitializerInAnotherModule(category: 0) // okay
  _ = DesignatedInitializerInAnotherModule(fromOtherModule: 0) // okay
}

func testSubclasses() {
  _ = IncompleteInitSubclass(first: 0) // okay
  _ = IncompleteInitSubclass(second: 0) // okay
  _ = IncompleteInitSubclass(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteInitSubclass(conveniently: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteInitSubclass(category: 0) // expected-error {{no exact matches in call to initializer}}

  _ = IncompleteInitSubclassImplicit(first: 0) // okay
  _ = IncompleteInitSubclassImplicit(second: 0) // okay
  _ = IncompleteInitSubclassImplicit(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteInitSubclassImplicit(conveniently: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteInitSubclassImplicit(category: 0) // expected-error {{no exact matches in call to initializer}}

  _ = IncompleteConvenienceInitSubclass(first: 0) // okay
  _ = IncompleteConvenienceInitSubclass(second: 0) // okay
  _ = IncompleteConvenienceInitSubclass(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteConvenienceInitSubclass(conveniently: 0) // okay
  _ = IncompleteConvenienceInitSubclass(category: 0) // okay

  _ = IncompleteUnknownInitSubclass(first: 0) // okay
  _ = IncompleteUnknownInitSubclass(second: 0) // okay
  _ = IncompleteUnknownInitSubclass(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteUnknownInitSubclass(conveniently: 0) // okay
  _ = IncompleteUnknownInitSubclass(category: 0) // okay

  _ = IncompleteInitCategorySubclass(first: 0) // okay
  _ = IncompleteInitCategorySubclass(second: 0) // okay
  _ = IncompleteInitCategorySubclass(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteInitCategorySubclass(conveniently: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteInitCategorySubclass(category: 0) // expected-error {{no exact matches in call to initializer}}

  _ = IncompleteInitCategorySubclassImplicit(first: 0) // okay
  _ = IncompleteInitCategorySubclassImplicit(second: 0) // okay
  _ = IncompleteInitCategorySubclassImplicit(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteInitCategorySubclassImplicit(conveniently: 0) // expected-error {{no exact matches in call to initializer}}
  _ = IncompleteInitCategorySubclassImplicit(category: 0) // expected-error {{no exact matches in call to initializer}}

  _ = DesignatedInitializerInAnotherModuleSubclass(first: 0) // okay
  _ = DesignatedInitializerInAnotherModuleSubclass(second: 0) // okay
  _ = DesignatedInitializerInAnotherModuleSubclass(missing: 0) // expected-error {{no exact matches in call to initializer}}
  _ = DesignatedInitializerInAnotherModuleSubclass(conveniently: 0) // expected-error {{no exact matches in call to initializer}}
  _ = DesignatedInitializerInAnotherModuleSubclass(category: 0) // expected-error {{no exact matches in call to initializer}}
  _ = DesignatedInitializerInAnotherModuleSubclass(fromOtherModule: 0) // okay
}
