//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import SafariServices // Clang module
import _SwiftSafariServicesOverlayShims

#if os(OSX)

@available(OSX, introduced: 10.11)
public func SFSafariServicesAvailable(_ version: SFSafariServicesVersion = SFSafariServicesVersion.version10_0) -> Bool {
  return _swift_SafariServices_isSafariServicesAvailable(version)
}

#endif
