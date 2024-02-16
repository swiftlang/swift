// REQUIRES: executable_test
// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module %S/Inputs/overlay.swift -module-name ClangModuleWithOverlay -I %S/Inputs -emit-module-path %t/ClangModuleWithOverlay.swiftmodule
// RUN: %target-build-swift -c %S/Inputs/overlay.swift -module-name ClangModuleWithOverlay -I %S/Inputs -o %t/ClangModuleWithOverlay.o -parse-as-library
// RUN: %target-build-swift -emit-executable %s %t/ClangModuleWithOverlay.o -I %t -g -o %t/ASTSectionOverlay -module-name ASTSectionOverlay -emit-module -Xlinker -add_ast_path -Xlinker %t/ClangModuleWithOverlay.swiftmodule

// RUN: %lldb-moduleimport-test -verbose %t/ASTSectionOverlay | %FileCheck %s
// CHECK: Loading ClangModuleWithOverlay
// CHECK-NOT: Loading (overlay) ClangModuleWithOverlay
// CHECK-NOT: Loading{{.*}}ClangModuleWithOverlay

import ClangModuleWithOverlay
let c = ClangType(i: 0)
fromSwiftOverlay()
