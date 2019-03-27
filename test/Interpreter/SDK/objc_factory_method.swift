// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name FactoryTest %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=macosx

// Requires swift-version 4
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import AppKit

let image = NSImage(named: NSImage.Name.trashEmpty)
// CHECK: TrashEmpty
print(image!.name()!)
