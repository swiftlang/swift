// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -o %t -module-name Test -enable-resilience
// RUN: %target-swift-ide-test -print-module -module-to-print Test -print-implicit-attrs -I %t -source-filename %s | %FileCheck %s

@frozen public enum Frozen {}
// CHECK-DAG: @frozen enum Frozen {
@_nonfrozen public enum NonFrozen {}
// CHECK-DAG: @_nonfrozen enum NonFrozen {
public enum Defaulted {}
// CHECK-DAG: @_nonfrozen enum Defaulted {
