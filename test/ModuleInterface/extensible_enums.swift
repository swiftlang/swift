// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library
// RUN: %target-swift-frontend -emit-module %t/src/Lib.swift \
// RUN:   -module-name Lib \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -enable-experimental-feature ExtensibleAttribute

// Check that the errors are produced when using enums from module with `ExtensibleEnums` feature enabled.
// RUN: %target-swift-frontend -typecheck %t/src/TestChecking.swift \
// RUN:   -swift-version 5 -module-name Client -I %t \
// RUN:   -verify

// Test to make sure that if the library and client are in the same package enums are checked exhaustively

/// Build the library
// RUN: %target-swift-frontend -emit-module %t/src/Lib.swift \
// RUN:   -module-name Lib \
// RUN:   -package-name Test \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -enable-experimental-feature ExtensibleAttribute


// Different module but the same package
// RUN: %target-swift-frontend -typecheck %t/src/TestSamePackage.swift \
// RUN:   -swift-version 5 -module-name Client -I %t \
// RUN:   -package-name Test \
// RUN:   -verify

// Different module but the same package
// RUN: %target-swift-frontend -typecheck %t/src/TestSwift6.swift \
// RUN:   -swift-version 6 -module-name Client -I %t \
// RUN:   -verify

// REQUIRES: swift_feature_ExtensibleAttribute

//--- Lib.swift

@extensible
public enum E {
  case a
}

@preEnumExtensibility
@extensible
public enum PE {
  case a
}

@frozen
public enum F {
  case a
  case b
}

func test_same_module(e: E, f: F) {
  switch e { // Ok
  case .a: break 
  }

  switch f { // Ok
  case .a: break
  case .b: break
  }
}

//--- TestChecking.swift
import Lib

func test(e: E, pe: PE, f: F) {
  // `E` is marked as `@extensible` which means it gets new semantics

  switch e {
  // expected-warning@-1 {{switch covers known cases, but 'E' may have additional unknown values, possibly added in future versions; this is an error in the Swift 6 language mode}}
  // expected-note@-2 {{handle unknown values using "@unknown default"}}
  case .a: break
  }

  switch e { // Ok (no warnings)
  case .a: break
  @unknown default: break
  }

  switch pe {
  // expected-warning@-1 {{switch covers known cases, but 'PE' may have additional unknown values, possibly added in future versions; this will be an error in a future Swift language mode}}
  // expected-note@-2 {{handle unknown values using "@unknown default"}}
  case .a: break
  }

  // `F` is marked as `@frozen` which means regular rules apply.

  switch f { // Ok (no errors because `F` is `@frozen`)
  case .a: break
  case .b: break
  }

  switch f { // expected-error {{switch must be exhaustive}} expected-note {{dd missing case: '.b'}}
  case .a: break
  }
  
  switch f { // expected-warning {{switch must be exhaustive}} expected-note {{dd missing case: '.b'}}
  case .a: break
  @unknown default: break
  }
}

//--- TestSamePackage.swift
import Lib

func test_no_default(e: E, f: F) {
  switch e { // Ok
  case .a: break
  }

  switch e { // expected-warning {{switch must be exhaustive}} expected-note {{dd missing case: '.a'}}
  @unknown default: break
  }

  switch f { // expected-error {{switch must be exhaustive}} expected-note {{dd missing case: '.b'}}
  case .a: break
  }

  switch f { // expected-warning {{switch must be exhaustive}} expected-note {{dd missing case: '.b'}}
  case .a: break
  @unknown default: break
  }
}

//--- TestSwift6.swift
import Lib

func test(e: E, pe: PE, f: F) {
  switch e {
  // expected-error@-1 {{switch covers known cases, but 'E' may have additional unknown values, possibly added in future versions}}
  // expected-note@-2 {{handle unknown values using "@unknown default"}}
  case .a: break
  }

  switch e { // Ok (no warnings)
  case .a: break
  @unknown default: break
  }

  switch pe {
  // expected-warning@-1 {{switch covers known cases, but 'PE' may have additional unknown values, possibly added in future versions; this will be an error in a future Swift language mode}}
  // expected-note@-2 {{handle unknown values using "@unknown default"}}
  case .a: break
  }
}
