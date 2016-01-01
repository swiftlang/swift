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

@_exported import Contacts
import Foundation

@available(OSX, introduced=10.11)
@available(iOS, introduced=9.0)
extension CNErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String { return CNErrorDomain }
}

