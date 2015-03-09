//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// FIXME: Function types don't work yet as generic parameters
public struct _REPLExitHandler {
  var f : () -> ()

  init(_ f: () -> ()) {
    self.f = f
  }
}

var _replExitHandlers = [_REPLExitHandler]()

public func _atREPLExit(handler: () -> ()) {
  _replExitHandlers.append(_REPLExitHandler(handler))
}

internal func _replExit() {
  for handler in lazy(_replExitHandlers).reverse() {
    handler.f()
  }
}
