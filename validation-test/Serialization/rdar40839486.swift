// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t/main4.swiftmodule -swift-version 4 -Fsystem %sdk/System/Library/PrivateFrameworks/ %s
// RUN: %target-build-swift -emit-module-path %t/main4_2.swiftmodule -swift-version 4.2 -Fsystem %sdk/System/Library/PrivateFrameworks/ %s

// REQUIRES: OS=macosx || OS=ios

import CloudKit

@available(macOS 10.10, iOS 8, *)
extension CKRecord {
  @inlinable public func testMethod() -> Any? {
    return self.object(forKey: "abc" as CKRecord.FieldKey)
  }

  @inlinable public func testSubscript() -> Any? {
    return self["abc" as CKRecord.FieldKey]
  }
}
