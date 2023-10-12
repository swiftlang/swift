// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend -sdk %S/Inputs -primary-file %s -trap-function oopsie -enable-objc-interop -emit-ir -module-name trap_function -I %t | %FileCheck %s -check-prefix=TRAPFN
// RUN: %target-swift-frontend -O -sdk %S/Inputs -primary-file %s -trap-function oopsie -enable-objc-interop -emit-ir -module-name trap_function -I %t | %FileCheck %s -check-prefix=TRAPFN_OPT
// RUN: %target-swift-frontend -sdk %S/Inputs -primary-file %s -enable-objc-interop -emit-ir -module-name trap_function -I %t | %FileCheck %s -check-prefix=NOTRAPFN
// REQUIRES: objc_codegen

import gizmo

// TRAPFN-LABEL: define hidden swiftcc void @"$s13trap_function18checkClangImporteryyF"
// TRAPFN: call void @llvm.trap() [[ATTR0:#[0-9]+]]

// TRAPFN_OPT-LABEL: define hidden swiftcc void @"$s13trap_function18checkClangImporteryyF"
// TRAPFN_OPT: call void @llvm.trap() [[ATTR0:#[0-9]+]]

// NOTRAPFN-LABEL: define hidden swiftcc void @"$s13trap_function18checkClangImporteryyF"
// NOTRAPFN: call void @llvm.trap(){{$}}
func checkClangImporter() {
  ackbar()
}

// TRAPFN: attributes [[ATTR0]] = { {{.*}}"trap-func-name"="oopsie" }
// TRAPFN_OPT: attributes [[ATTR0]] = { {{.*}}"trap-func-name"="oopsie" }
// NOTRAPFN-NOT: attributes {{.*}} = { {{.*}}"trap-func-name"="oopsie" }
