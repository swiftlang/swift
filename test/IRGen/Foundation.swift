// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s

// Fake Foundation module for testing String/NSString bridging.

import Foundation // clang module

func [asmname="swift_StringToNSString"]
convertStringToNSString(string : [byref] String) -> NSString

func [asmname="swift_NSStringToString"]
convertNSStringToString(nsstring : NSString, string : [byref] String)


