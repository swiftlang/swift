// RUN: %target-parse-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import CoreServices

func test(url: CFURL, ident: CSIdentity) {
  _ = CSBackupIsItemExcluded(url, nil) // okay

  _ = nil as Collection? // expected-error {{use of undeclared type 'Collection'}}
  _ = nil as CoreServices.Collection? // okay

  _ = kCollectionNoAttributes // expected-error{{use of unresolved identifier 'kCollectionNoAttributes'}}

  var name: Unmanaged<CFString>? = nil
  _ = LSCopyDisplayNameForURL(url, &name) as OSStatus // okay

  let unicharArray: [UniChar] = [ 0x61, 0x62, 0x63, 0x2E, 0x64 ]
  var extIndex: Int = 0
  LSGetExtensionInfo(unicharArray.count, unicharArray, &extIndex) // okay

  _ = CSIdentityCreateCopy(nil, ident) // okay

  var vers: UInt32 = 0
  _ = KCGetKeychainManagerVersion(&vers) as OSStatus// expected-error{{use of unresolved identifier 'KCGetKeychainManagerVersion'}}
  _ = CoreServices.KCGetKeychainManagerVersion(&vers) as OSStatus// okay
}
