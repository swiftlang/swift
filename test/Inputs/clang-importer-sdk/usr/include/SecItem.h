// The name of this header used to be special-cased in the Clang importer.
@import ctypes;
@import Foundation; // no CoreFoundation in our mock SDK

extern const CFStringRef kSecClass;
extern const CFStringRef kSecClassGenericPassword;

OSStatus SecItemCopyMatching(CFDictionaryRef query, CFTypeRef *result);
