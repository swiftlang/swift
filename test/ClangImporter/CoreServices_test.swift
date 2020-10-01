// RUN: %target-typecheck-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import CoreServices

func test(_ url: CFURL, ident: CSIdentity) {
  _ = CSBackupIsItemExcluded(url, nil) // okay

  _ = nil as TypeThatDoesNotExist? // expected-error {{cannot find type 'TypeThatDoesNotExist' in scope}}
  _ = nil as CoreServices.Collection? // okay

  _ = kCollectionNoAttributes // expected-error{{cannot find 'kCollectionNoAttributes' in scope}}

  var name: Unmanaged<CFString>?
  _ = LSCopyDisplayNameForURL(url, &name) as OSStatus // okay

  let unicharArray: [UniChar] = [ 0x61, 0x62, 0x63, 0x2E, 0x64 ]
  var extIndex: Int = 0
  LSGetExtensionInfo(unicharArray.count, unicharArray, &extIndex) // okay

  _ = CSIdentityCreateCopy(nil, ident) // okay

  var vers: UInt32 = 0
  _ = KCGetKeychainManagerVersion(&vers) as OSStatus// expected-error{{cannot find 'KCGetKeychainManagerVersion' in scope}}
  _ = CoreServices.KCGetKeychainManagerVersion(&vers) as OSStatus// okay
}
