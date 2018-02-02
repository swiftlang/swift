// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -print-implicit-attrs=true -swift-version 4 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-4 %s
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -print-implicit-attrs=true -swift-version 5 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-5 %s

@frozen public enum Frozen {}
// CHECK-LABEL: @frozen public enum Frozen {
@_nonfrozen public enum NonFrozen {}
// CHECK-LABEL: @_nonfrozen public enum NonFrozen {
public enum Defaulted {}
// CHECK-4-LABEL: @frozen public enum Defaulted {
// CHECK-5-LABEL: @_nonfrozen public enum Defaulted {

@frozen @frozen public enum Duplicate {} // expected-error {{duplicate attribute}}
// expected-note@-1 {{attribute already specified here}}

@frozen @_nonfrozen public enum Both1 {} // expected-error {{enum cannot be both '@frozen' and '@_nonfrozen'}} {{1-9=}}
@_nonfrozen @frozen public enum Both2 {} // expected-error {{enum cannot be both '@frozen' and '@_nonfrozen'}} {{13-21=}}

@frozen enum NotPublic {} // expected-warning {{@frozen has no effect on non-public enums}} {{1-9=}}
@_nonfrozen enum AlsoNotPublic {} // expected-warning {{@_nonfrozen has no effect on non-public enums}} {{1-13=}}

internal enum Outer {
  @frozen public enum ButThisIsOK {} // no-warning
}

@_versioned @frozen enum NotPublicButVersioned {} // no-warning
