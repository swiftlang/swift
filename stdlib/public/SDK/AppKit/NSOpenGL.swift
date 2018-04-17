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

import Foundation
@_exported import AppKit

extension NSOpenGLGlobalOption {
  @available(swift 4)
  public var globalValue: GLint {
    get {
      var value: GLint = 0
      __NSOpenGLGetOption(self, &value)
      return value
    }
    set {
      __NSOpenGLSetOption(self, newValue)
    }
  }
}

extension NSOpenGLContext {
  @available(swift 4)
  public static var openGLVersion: (major: GLint, minor: GLint) {
    var major: GLint = 0
    var minor: GLint = 0
    __NSOpenGLGetVersion(&major, &minor)
    return (major: major, minor: minor)
  }
}
