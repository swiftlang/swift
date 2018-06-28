// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 4 -F %sdk/System/Library/PrivateFrameworks %s -o %t/a.out-4 && %target-run %t/a.out-4
// RUN: %target-build-swift -swift-version 4.2 -F %sdk/System/Library/PrivateFrameworks %s -o %t/a.out-4.2 && %target-run %t/a.out-4.2
// REQUIRES: executable_test
// REQUIRES: objc_interop

import CloudKit
import StdlibUnittest
import StdlibUnittestFoundationExtras

let CloudKitTests = TestSuite("CloudKit")


CloudKitTests.test("Type renames") {
  if #available(macOS 10.10, iOS 8.0, tvOS 9.0, watchOS 3.0, *) {
#if swift(>=4.2)
    let _: CKRecord.ID? = nil
    let _: CKRecord.Reference? = nil
    let _: CKRecordZone.ID? = nil
    let _: CKNotification.ID? = nil
    let _: CKQueryOperation.Cursor? = nil
    let _: CKModifyRecordsOperation.RecordSavePolicy? = nil
    let _: CKNotification.NotificationType? = nil
    let _: CKQueryNotification.Reason? = nil
    let _: CKRecordZone.Capabilities? = nil
#else
    let _: CKRecordID? = nil
    let _: CKReference? = nil
    let _: CKRecordZoneID? = nil
    let _: CKNotificationID? = nil
    let _: CKQueryCursor? = nil
    let _: CKRecordSavePolicy? = nil
    let _: CKNotificationType? = nil
    let _: CKQueryNotificationReason? = nil
    let _: CKRecordZoneCapabilities? = nil
#endif
  }

  if #available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *) {
#if swift(>=4.2)
    let _: CKShare.Participant? = nil
    let _: CKUserIdentity.LookupInfo? = nil 
    let _: CKShare.Metadata? = nil
    let _: CKDatabase.Scope? = nil
#else
    let _: CKShareParticipant? = nil
    let _: CKUserIdentityLookupInfo? = nil
    let _: CKShareMetadata? = nil
    let _: CKDatabaseScope? = nil
#endif
  }

  if #available(macOS 10.13, iOS 11.0, tvOS 11.0, watchOS 4.0, *) {
#if swift(>=4.2)
    let _: CKOperation.Configuration? = nil
    let _: CKOperationGroup.TransferSize? = nil
#else
    let _: CKOperationConfiguration? = nil
    let _: CKOperationGroupTransferSize? = nil
#endif
  }

#if !os(watchOS)
  if #available(macOS 10.10, iOS 8.0, tvOS 9.0, *) {
#if swift(>=4.2)
    let _: CKSubscription.SubscriptionType? = nil
    let _: CKSubscription.NotificationInfo? = nil
#else
    let _: CKSubscriptionType? = nil
    let _: CKNotificationInfo? = nil
#endif // swift(>=4.2)
  }

  if #available(macOS 10.12, iOS 10.0, tvOS 10.0, *) {
#if swift(>=4.2)
    let _: CKQuerySubscription.Options? = nil
#else
    let _: CKQuerySubscriptionOptions? = nil
#endif // swift(>=4.2)
  }
#endif // !os(watchOS)
}

runAllTests() 
