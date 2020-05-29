// RUN: %empty-directory(%t)

// Make sure we don't crash if we fail to load a partial module.
// RUN: %target-swift-frontend -emit-module -primary-file %s -module-name A -o %t/a.partial.swiftmodule
// RUN: not %target-swift-frontend -emit-module -merge-modules %t/a.partial.swiftmodule -module-name Unrelated 2>&1 | %FileCheck %s

// CHECK: error: cannot load module 'A' as 'Unrelated'
