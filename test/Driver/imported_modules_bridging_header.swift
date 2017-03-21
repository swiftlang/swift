// RUN: not %target-build-swift -typecheck %s -import-objc-header %S/Inputs/imported_modules/bridging_header.h -I %S/Inputs/imported_modules 2>&1 | %FileCheck %s
// RUN: %target-build-swift -emit-imported-modules %s -o %t.importedmodules -import-objc-header %S/Inputs/imported_modules/bridging_header.h -I %S/Inputs/imported_modules
// RUN: diff %t.importedmodules %S/Inputs/imported_modules/imported_modules_bridging_header.importedmodules

// Confirm that the compiler does look for/find the bad overlay during a normal
// compilation, and thus validate that -emit-imported-modules doesn't.
// CHECK: malformed module file: {{.*}}InvalidOverlay.swiftmodule

// REQUIRES: objc_interop
