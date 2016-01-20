// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse %s -verify

// REQUIRES: objc_interop

import Security

_ = kSecClass as CFStringRef
_ = kSecClassGenericPassword as CFStringRef
_ = kSecClassGenericPassword as CFDictionaryRef // expected-error {{'CFString!' is not convertible to 'CFDictionaryRef'}} {{30-32=as!}}

func testIntegration() {
  // Based on code in <rdar://problem/17162475>.
  let query = [kSecClass as NSString: kSecClassGenericPassword] as NSDictionary as CFDictionary

  var dataTypeRef: Unmanaged<AnyObject>? = nil
  let status = SecItemCopyMatching(query, &dataTypeRef)
  
  if status == errSecSuccess {
    if let filledRef = dataTypeRef {
      let str: NSString = filledRef.takeRetainedValue() as! NSString
      print("Got: \(str)")
    }
  }
}

func testAuthorizationIsNotCF() {
  var auth = AuthorizationRef() // expected-warning {{deprecated}}
  _ = AuthorizationCreate(&auth)
  _ = AuthorizationFree(auth)
}
