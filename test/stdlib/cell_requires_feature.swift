// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-stdlib -emit-module-path %t -module-name Swift %S/Inputs/CellFakeStdlib.swift
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -nostdimport -I %t -verify-additional-prefix missing-
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -nostdimport -I %t -enable-experimental-feature Cells
// 
// REQUIRES: swift_feature_Cells

@available(SwiftStdlib 6.4, *)
func f(_: borrowing Cell<Int>) { }
// expected-missing-error@-1{{using 'Cell' requires an experimental feature; use '-enable-experimental-feature Cells'}}


@available(SwiftStdlib 6.4, *)
func g(_: borrowowing ConstCell<Int>) { }
// expected-missing-error@-1{{using 'ConstCell' requires an experimental feature; use '-enable-experimental-feature Cells'}}
