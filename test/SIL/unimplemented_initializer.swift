// RUN: %target-swift-frontend -sdk %S/../SILGen/Inputs -I %S/../SILGen/Inputs -enable-source-import -primary-file %s -emit-sil -emit-verbose-sil | FileCheck %s -check-prefix=CHECK-DEBUG
// RUN: %target-swift-frontend -sdk %S/../SILGen/Inputs -I %S/../SILGen/Inputs -enable-source-import -primary-file %s -emit-sil -emit-verbose-sil -O | FileCheck %s -check-prefix=CHECK-RELEASE

// XFAIL: linux

import gizmo

// Test that binaries compiled in release mode don't leak filenames of users'
// code through calls to runtime trap function for unimplemented initializers.

class DesignatedStubs : Gizmo {
  override init() { super.init() }
}
// CHECK-DEBUG: sil hidden @_TFC25unimplemented_initializer15DesignatedStubscfMS0_FT7bellsOnSi_GSQS0__
// CHECK-DEBUG: string_literal utf8 "{{.*}}unimplemented_initializer.swift"
// CHECK-DEBUG: function_ref @_TFVSs12StaticString14withUTF8BufferfS_U__FFGVSs19UnsafeBufferPointerVSs5UInt8_Q_Q_

// CHECK-RELEASE: sil hidden @_TFC25unimplemented_initializer15DesignatedStubscfMS0_FT7bellsOnSi_GSQS0__
// CHECK-RELEASE-NOT: unimplemented_initializer.swift"

