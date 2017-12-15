// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -print-implicit-attrs=true -swift-version 4 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-4 %s
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -print-implicit-attrs=true -swift-version 5 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-5 %s

@_exhaustive public enum Exhaustive {}
// CHECK-LABEL: @_exhaustive public enum Exhaustive {
@_nonexhaustive public enum NonExhaustive {}
// CHECK-LABEL: @_nonexhaustive public enum NonExhaustive {
public enum Defaulted {}
// CHECK-4-LABEL: @_exhaustive public enum Defaulted {
// CHECK-5-LABEL: @_nonexhaustive public enum Defaulted {

@_exhaustive @_exhaustive public enum Duplicate {} // expected-error {{duplicate attribute}}
// expected-note@-1 {{attribute already specified here}}

@_exhaustive @_nonexhaustive public enum Both1 {} // expected-error {{enum cannot be both '@exhaustive' and '@nonexhaustive'}} {{1-14=}}
@_nonexhaustive @_exhaustive public enum Both2 {} // expected-error {{enum cannot be both '@exhaustive' and '@nonexhaustive'}} {{17-30=}}

@_exhaustive enum NotPublic {} // expected-warning {{@_exhaustive has no effect on non-public enums}} {{1-14=}}
@_nonexhaustive enum AlsoNotPublic {} // expected-warning {{@_nonexhaustive has no effect on non-public enums}} {{1-17=}}

internal enum Outer {
  @_exhaustive public enum ButThisIsOK {} // no-warning
}

@_versioned @_exhaustive enum NotPublicButVersioned {} // no-warning
