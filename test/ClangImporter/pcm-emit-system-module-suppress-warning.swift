// Test that warnings from system modules are suppressed during PCM generation

// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-pcm -module-name SystemModuleWithWarnings \
// RUN:   -Xcc -Xclang -Xcc -emit-module \
// RUN:   -Xcc -Xclang -Xcc -fsystem-module \
// RUN:   -o %t/SystemModuleWithWarnings.pcm \
// RUN:   %S/Inputs/custom-modules/module.modulemap 2>&1 | %FileCheck %s --allow-empty

// Verify NO warnings are emitted for system modules
// CHECK-NOT: warning:

// Emit PCM for NON-system module - should show warnings
// RUN: %target-swift-emit-pcm -module-name SystemModuleWithWarnings \
// RUN:   -o %t/SystemModuleWithWarnings.pcm \
// RUN:   %S/Inputs/custom-modules/module.modulemap 2>&1 | %FileCheck %s --check-prefix=CHECK-WARNINGS

// CHECK-WARNINGS: warning:

import SystemModuleWithWarnings
