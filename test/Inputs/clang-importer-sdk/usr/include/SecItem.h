// The name of this header is special-cased in the Clang importer. :-(
@import Foundation; // no CoreFoundation in our mock SDK

extern const CFTypeRef kSecClass;
extern /*const*/ CFTypeRef kSecClassGenericPassword;

unsigned SecItemCopyMatching(CFDictionaryRef query, CFTypeRef *result);
