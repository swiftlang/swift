// RUN: %target-parse-verify-swift %clang-importer-sdk

import CoreServices

func test(url: CFURL, ident: CSIdentity) {
  _ = CSBackupIsItemExcluded(url, nil) // okay

  let _: Collection? = nil // expected-error {{use of undeclared type 'Collection'}}
  let _: CoreServices.Collection? = nil // okay

  _ = kCollectionNoAttributes // expected-error{{use of unresolved identifier 'kCollectionNoAttributes'}}

  var name: Unmanaged<CFString>? = nil
  let _: OSStatus = LSCopyDisplayNameForURL(url, &name) // okay

  let unicharArray: [UniChar] = [ 0x61, 0x62, 0x63, 0x2E, 0x64 ]
  var extIndex: Int = 0
  LSGetExtensionInfo(unicharArray.count, unicharArray, &extIndex) // okay

  _ = CSIdentityCreateCopy(nil, ident) // okay

  var vers: UInt32 = 0
  let _: OSStatus = KCGetKeychainManagerVersion(&vers) // expected-error{{use of unresolved identifier 'KCGetKeychainManagerVersion'}}
  let _: OSStatus = CoreServices.KCGetKeychainManagerVersion(&vers) // okay
}
