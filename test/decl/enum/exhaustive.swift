// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -print-implicit-attrs=true -swift-version 4 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-4 %s
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -print-implicit-attrs=true -swift-version 5 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-5 %s

public _exhaustive enum Exhaustive {}
// CHECK-LABEL: public _exhaustive enum Exhaustive {
public _nonexhaustive enum NonExhaustive {}
// CHECK-LABEL: public _nonexhaustive enum NonExhaustive {
public enum Defaulted {}
// CHECK-4-LABEL: public _exhaustive enum Defaulted {
// CHECK-5-LABEL: public _nonexhaustive enum Defaulted {

public _exhaustive _exhaustive enum Duplicate {} // expected-error {{duplicate modifier}}
// expected-note@-1 {{modifier already specified here}}

public _exhaustive _nonexhaustive enum Both1 {} // expected-error {{enum cannot be both 'exhaustive' and 'nonexhaustive'}} {{8-20=}}
public _nonexhaustive _exhaustive enum Both2 {} // expected-error {{enum cannot be both 'exhaustive' and 'nonexhaustive'}} {{23-35=}}

_exhaustive enum NotPublic {} // expected-warning {{'_exhaustive' has no effect on non-public enums}} {{1-13=}}
_nonexhaustive enum AlsoNotPublic {} // expected-warning {{'_nonexhaustive' has no effect on non-public enums}} {{1-16=}}

internal enum Outer {
  _exhaustive public enum ButThisIsOK {} // no-warning
}

@_versioned _exhaustive enum NotPublicButVersioned {} // no-warning
