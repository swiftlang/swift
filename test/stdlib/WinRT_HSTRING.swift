// RUN: %target-build-swift %s
// REQUIRES: OS=windows-msvc

// Make sure that importing WinRT brings in the HSTRING type.

import WinRT

public func usesHSTRING(_ x: HSTRING) {}
