// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/../SILGen/Inputs -I %S/../SILGen/Inputs -enable-source-import %s -emit-sil -emit-verbose-sil | FileCheck %s -check-prefix=CHECK-DEBUG
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/../SILGen/Inputs -I %S/../SILGen/Inputs -enable-source-import %s -emit-sil -emit-verbose-sil -O | FileCheck %s -check-prefix=CHECK-RELEASE

import gizmo

// Test that binaries compiled in release mode don't leak filenames of users'
// code through calls to runtime trap function for unimplemented initializers.

class DesignatedStubs : Gizmo {
  init() { super.init() }
}
// CHECK-DEBUG: sil @_TFC25unimplemented_initializer15DesignatedStubscfMS0_FT7bellsOnSi_S0_
// CHECK-DEBUG: string_literal utf8 "{{.*}}unimplemented_initializer.swift"
// CHECK-DEBUG: function_ref @swift_reportUnimplementedInitializerInFile

// CHECK-RELEASE: sil @_TFC25unimplemented_initializer15DesignatedStubscfMS0_FT7bellsOnSi_S0_
// CHECK-RELEASE-NOT: unimplemented_initializer.swift
// CHECK-RELEASE: function_ref @swift_reportUnimplementedInitializer
// CHECK-RELEASE-NOT: unimplemented_initializer.swift

