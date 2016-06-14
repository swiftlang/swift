//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import SafariServices // Clang module

@_silgen_name("_swift_SafariServices_isSafariServicesAvailable")
internal func _isSafariServicesAvailable() -> Bool

@available(OSX, introduced: 10.11)
public func SFSafariServicesAvailable() -> Bool {
  return _isSafariServicesAvailable()
}
