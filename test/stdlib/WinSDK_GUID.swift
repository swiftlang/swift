// RUN: %target-build-swift %s
// REQUIRES: OS=windows-msvc

// Make sure that importing WinSDK brings in the GUID type, which is declared in
// /shared and not in /um.

import WinSDK

public func usesGUID(_ x: GUID) {}
