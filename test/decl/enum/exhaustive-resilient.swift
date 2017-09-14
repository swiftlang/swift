// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -o %t -module-name Test -enable-resilience
// RUN: %target-swift-ide-test -print-module -module-to-print Test -print-implicit-attrs -I %t -source-filename %s | %FileCheck %s

public _exhaustive enum Exhaustive {}
// CHECK-DAG: _exhaustive enum Exhaustive {
public _nonexhaustive enum NonExhaustive {}
// CHECK-DAG: _nonexhaustive enum NonExhaustive {
public enum Defaulted {}
// CHECK-DAG: _nonexhaustive enum Defaulted {
