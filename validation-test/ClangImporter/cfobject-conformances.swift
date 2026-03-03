// RUN: %target-swift-frontend -emit-sil %s -verify
// RUN: %target-swift-frontend -emit-sil %s -enable-upcoming-feature MemberImportVisibility -verify

// REQUIRES: VENDOR=apple
// REQUIRES: swift_feature_MemberImportVisibility

import CoreFoundation

public func takesHashable<T: Hashable>(_ t: T) {}

public func takesCFObjects(
  _ string: CFString,
  _ number: CFNumber,
  _ date: CFDate,
  _ data: CFData,
  _ set: CFSet,
) {
  takesHashable(string)
  takesHashable(number)
  takesHashable(date)
  takesHashable(data)
  takesHashable(set)
}
