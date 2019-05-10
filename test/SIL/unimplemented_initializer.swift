// RUN: %target-swift-frontend -sdk %S/../SILGen/Inputs -I %S/../SILGen/Inputs -Xllvm -sil-print-debuginfo -enable-objc-interop -enable-source-import -primary-file %s -emit-sil -emit-verbose-sil | %FileCheck %s -check-prefix=CHECK-DEBUG
// RUN: %target-swift-frontend -sdk %S/../SILGen/Inputs -I %S/../SILGen/Inputs -Xllvm -sil-print-debuginfo -enable-objc-interop -enable-source-import -primary-file %s -emit-sil -emit-verbose-sil -O | %FileCheck %s -check-prefix=CHECK-RELEASE

import gizmo

// Test that binaries compiled in release mode don't leak filenames of users'
// code through calls to runtime trap function for unimplemented initializers.

class DesignatedStubs : Gizmo {
  override init() { super.init() }
}
// CHECK-DEBUG: string_literal utf8 "{{.*}}unimplemented_initializer.swift",

// CHECK-RELEASE-NOT: unimplemented_initializer.swift",

