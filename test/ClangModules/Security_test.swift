// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify

import Security

let _: CFStringRef = kSecClass
let _: CFStringRef = kSecClassGenericPassword
let _: CFDictionaryRef = kSecClassGenericPassword // expected-error {{'CFStringRef' is not convertible to 'CFDictionaryRef'}}

func testIntegration() {
  // Based on code in <rdar://problem/17162475>.
  let query = [kSecClass as NSString: kSecClassGenericPassword] as NSDictionary as CFDictionary

  var dataTypeRef: Unmanaged<AnyObject>? = nil
  let status = SecItemCopyMatching(query, &dataTypeRef)
  
  if status == errSecSuccess {
    if let filledRef = dataTypeRef {
      let str: NSString = filledRef.takeRetainedValue() as! NSString
      println("Got: \(str)")
    }
  }
}

func testAuthorizationIsNotCF() {
  var auth = AuthorizationRef()
  _ = AuthorizationCreate(&auth)
  _ = AuthorizationFree(auth)
}
