// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %clang-importer-sdk -emit-module -o %t %s
// RUN: %target-swift-ide-test -print-module -module-to-print=print -I %t -source-filename=%s %clang-importer-sdk | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: class PropertyOwnership {
class PropertyOwnership {
  // CHECK-NEXT:  @NSCopying var copying: NSCopying?
  @NSCopying var copying: NSCopying?
  // CHECK-NEXT:  weak var weakVar: @sil_weak AnyObject?
  weak var weakVar: AnyObject?
  // CHECK-NEXT:  unowned var unownedVar: @sil_unowned PropertyOwnership
  unowned var unownedVar: PropertyOwnership
  // CHECK-NEXT:  unowned(unsafe) var unownedUnsafeVar: @sil_unmanaged PropertyOwnership
  unowned(unsafe) var unownedUnsafeVar: PropertyOwnership

  // CHECK-NEXT:  init(other: PropertyOwnership)
  init(other: PropertyOwnership) {
    unownedVar = other
    unownedUnsafeVar = other
  }
  // CHECK-NEXT:  deinit
} // CHECK-NEXT: }
