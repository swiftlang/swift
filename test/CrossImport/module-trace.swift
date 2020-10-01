// This file tests that we emit cross-imports into module interfaces.

// FIXME: Fix the path normalization issue and get rid of this.
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/

// RUN: %target-swift-frontend -enable-cross-import-overlays -I %t/lib/swift -typecheck %s -module-name main -swift-version 5 -emit-loaded-module-trace-path - | %FileCheck %s

import DeclaringLibrary
import BystandingLibrary

// CHECK-DAG: {"name":"DeclaringLibrary","path":"{{[^"]+}}","isImportedDirectly":true
// CHECK-DAG: {"name":"BystandingLibrary","path":"{{[^"]+}}","isImportedDirectly":true
// CHECK-DAG: {"name":"_OverlayLibrary","path":"{{[^"]+}}","isImportedDirectly":true
