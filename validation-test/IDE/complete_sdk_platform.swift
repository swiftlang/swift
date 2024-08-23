// RUN: %empty-directory(%t)
// RUN: ln -s %sdk %t/sdk

// RUN: %batch-code-completion -F %xcode-extra-frameworks-dir -I %xcode-extra-platform-swift-modules

// Works if SDK is specified as a symlink too.
// RUN: %batch-code-completion -sdk %t/sdk -F %xcode-extra-frameworks-dir -I %xcode-extra-platform-swift-modules

// REQUIRES: VENDOR=apple

// rdar://131854240 - Make sure modules found in the platform dir are treated
// as system.

import XCTest

#^COMPLETE^#
// COMPLETE: Decl[Module]/None/IsSystem: XCTest[#Module#]; name=XCTest
// COMPLETE: Decl[FreeFunction]/OtherModule[XCTest]/IsSystem: XCTFail()[#Void#]; name=XCTFail()
