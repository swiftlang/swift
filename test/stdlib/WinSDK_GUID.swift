// RUN: %target-build-swift %s -o %t.exe
// RUN: %target-codesign %t.exe
// RUN: %target-run %t.exe
// REQUIRES: executable_test
// REQUIRES: OS=windows-msvc

// Make sure that importing WinSDK brings in the GUID type from the external
// WinSDK.Shared component map.

import WinSDK

public func usesGUID(_ x: GUID) {}

// Make sure equating and hashing GUIDs works.

let guid: GUID = GUID_NULL
assert(guid == guid)
assert(guid.hashValue == guid.hashValue)

let guid2: GUID = IID_IUnknown
assert(guid != guid2)
assert(guid.hashValue != guid2.hashValue) // well, probably
