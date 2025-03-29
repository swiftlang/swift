// RUN: %empty-directory(%t)
// RUN: ln -s %sdk %t/sdk

// RUN: %batch-code-completion %xcode-extra-platform-search-paths

// Works if SDK is specified as a symlink too.
// RUN: %batch-code-completion -sdk %t/sdk %xcode-extra-platform-search-paths

// REQUIRES: OS=macosx
// rdar://131854240 - Make sure modules found in the platform dir are treated
// as system.

import XCTest

#^COMPLETE^#
// COMPLETE: Decl[Module]/None/IsSystem: XCTest[#Module#]; name=XCTest
// COMPLETE: Decl[FreeFunction]/OtherModule[XCTest]/IsSystem: XCTFail()[#Void#]; name=XCTFail()
