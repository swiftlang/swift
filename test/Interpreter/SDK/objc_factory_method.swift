// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift -module-name FactoryTest %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import AppKit

let image = NSImage(named: NSImageNameTrashEmpty)
// CHECK: TrashEmpty
print(image!.name()!)
