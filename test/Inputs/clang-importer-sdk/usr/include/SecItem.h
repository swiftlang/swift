@import Foundation; // no CoreFoundation in our mock SDK

extern const CFTypeRef kSecClass;
extern /*const*/ CFTypeRef kSecClassGenericPassword;

unsigned SecItemCopyMatching(CFDictionaryRef query, CFTypeRef *result);
