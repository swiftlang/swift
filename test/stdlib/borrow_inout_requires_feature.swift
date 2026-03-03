// RUN: %empty-directory(%t)
// RUN: %target-build-swift -enable-experimental-feature Lifetimes -parse-stdlib -emit-module-path %t -module-name Swift %S/Inputs/BorrowInoutFakeStdlib.swift
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -nostdimport -I %t -verify-additional-prefix missing-
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -nostdimport -I %t -enable-experimental-feature BorrowInout
// 
// REQUIRES: swift_feature_BorrowInout
// REQUIRES: swift_feature_Lifetimes

@available(SwiftStdlib 6.4, *)
func f(_: Borrow<Int>) { }
// expected-missing-error@-1{{'Borrow' is an experimental feature}}


@available(SwiftStdlib 6.4, *)
func g(_: consuming Inout<Int>) { }
// expected-missing-error@-1{{'Inout' is an experimental feature}}

