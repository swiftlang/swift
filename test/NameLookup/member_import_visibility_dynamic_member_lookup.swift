// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Lib.swiftmodule -parse-as-library %t/Lib.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Ext.swiftmodule -parse-as-library -I %t %t/Ext.swift

// RUN: %target-swift-frontend -typecheck -verify -swift-version 6 -I %t -primary-file %t/Client.swift %t/LoadsExt.swift -enable-upcoming-feature MemberImportVisibility -verify-additional-prefix import-
// RUN: %target-swift-frontend -typecheck -verify -swift-version 6 -I %t -primary-file %t/Client.swift %t/LoadsExt.swift -enable-upcoming-feature MemberImportVisibility:migrate -verify-additional-prefix migrate-

// REQUIRES: swift_feature_MemberImportVisibility

//--- Lib.swift

public struct ScopeA { public var a: Int { 0 } }

@dynamicMemberLookup
public struct Lookup {
  public subscript<T>(dynamicMember kp: KeyPath<ScopeA, T>) -> T { fatalError() }
}

@dynamicMemberLookup
public struct Box {
  public init() {}
  public subscript<T>(dynamicMember kp: KeyPath<Lookup, T>) -> Int { get { 0 } set {} }
}

public struct Thing { public init() {} }

//--- Ext.swift

import Lib

public struct ScopeB { public var b: Int { 0 } }

extension Lookup {
  public subscript<T>(dynamicMember kp: KeyPath<ScopeB, T>) -> T { fatalError() }
}

extension Thing {
  public static var ext: Thing { Thing() }
  public var prop: Int { 0 }
}

//--- LoadsExt.swift

import Ext

//--- Client.swift

import Lib

// expected-import-note@-1 9 {{add import of module 'Ext'}}{{1-1=import Ext\n}}
// expected-migrate-warning@-2 {{import of module 'Ext' is required}}{{1-1=import Ext\n}}

// The implicit subscript(dynamicMember:) in a chain of dynamic member lookups is diagnosed at the member access: the
// `.` in an expression (like the outer subscript), or the member name in a key path literal.
func testChainedDynamicMemberWrite() {
  var box = Box()
  box.b = 1
  // expected-import-error@-1:6 {{subscript 'subscript(dynamicMember:)' is not available due to missing import of defining module 'Ext'}}
  // expected-import-error@-2:7 {{property 'b' is not available due to missing import of defining module 'Ext'}}
  // expected-migrate-note@-3:6 {{subscript 'subscript(dynamicMember:)' from 'Ext' used here}}
  // expected-migrate-note@-4:7 {{property 'b' from 'Ext' used here}}
}

func testChainedDynamicMemberRead() -> Int {
  Box().b
  // expected-import-error@-1:8 {{subscript 'subscript(dynamicMember:)' is not available due to missing import of defining module 'Ext'}}
  // expected-import-error@-2:9 {{property 'b' is not available due to missing import of defining module 'Ext'}}
  // expected-migrate-note@-3:8 {{subscript 'subscript(dynamicMember:)' from 'Ext' used here}}
  // expected-migrate-note@-4:9 {{property 'b' from 'Ext' used here}}
}

func testChainedDynamicMemberKeyPathLiteral() -> KeyPath<Box, Int> {
  \Box.b
  // expected-import-error@-1:8 {{subscript 'subscript(dynamicMember:)' is not available due to missing import of defining module 'Ext'}}
  // expected-import-error@-2:8 {{property 'b' is not available due to missing import of defining module 'Ext'}}
  // expected-migrate-note@-3:8 {{subscript 'subscript(dynamicMember:)' from 'Ext' used here}}
  // expected-migrate-note@-4:8 {{property 'b' from 'Ext' used here}}
}

// References wrapped in implicit conversions are diagnosed once.
func testImplicitConversionsAreDiagnosedOnce() -> (Thing?, Int?, Any) {
  let a: Thing? = .ext
  // expected-import-error@-1 {{static property 'ext' is not available due to missing import of defining module 'Ext'}}
  // expected-migrate-note@-2 {{static property 'ext' from 'Ext' used here}}
  let b: Int? = Thing().prop
  // expected-import-error@-1 {{property 'prop' is not available due to missing import of defining module 'Ext'}}
  // expected-migrate-note@-2 {{property 'prop' from 'Ext' used here}}
  let c: Any = (Thing().prop)
  // expected-import-error@-1 {{property 'prop' is not available due to missing import of defining module 'Ext'}}
  // expected-migrate-note@-2 {{property 'prop' from 'Ext' used here}}
  return (a, b, c)
}
