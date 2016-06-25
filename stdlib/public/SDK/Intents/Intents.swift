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

@_exported import Intents
import Foundation

// FIXME: update the availability attributes, when
//        the underlying framework has them in ObjC
// @available(iOS, introduced: 10.0)
// @available(OSX, introduced: 10.12)
// @available(tvOS, introduced: 10.0)
// @available(watchOS, introduced: 3.0)
extension INIntentErrorCode : _BridgedNSError {
  public static var _nsErrorDomain: String { return INIntentErrorDomain }
}
