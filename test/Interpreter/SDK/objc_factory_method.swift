// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/cache)
// RUN: %target-build-swift -module-name FactoryTest %s -o %t/a.out -module-cache-path %t/cache
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import AppKit

let image = NSImage(named: NSImage.Name.trashEmpty)
// CHECK: TrashEmpty
print(image!.name()!)
