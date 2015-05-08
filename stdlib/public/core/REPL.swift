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
  let reversed = lazy(_replExitHandlers).reverse()
  for handler in reversed {
    handler.f()
  }
}

/// Print a string as is to stdout.
public // COMPILER_INTRINSIC
func _replPrintLiteralString(text: String) {
  var target = _Stdout()
  target._lock()
  _print_unlocked(text, &target)
  target._unlock()
  // FIXME: <rdar://problem/20812952> _replPrintLiteralString and _replDebugPrintln should use print()
  //
  // Should be:
  // print(text, appendNewline: false)
}

/// Print the debug representation of `value`, followed by a newline.
@inline(never)
@_semantics("stdlib_binary_only")
public // COMPILER_INTRINSIC
func _replDebugPrintln<T>(value: T) {
  var target = _Stdout()
  target._lock()
  _debugPrint_unlocked(value, &target)
  target.write("\n")
  target._unlock()
  // FIXME: <rdar://problem/20812952> _replPrintLiteralString and _replDebugPrintln should use print()
  //
  // Should be:
  // debugPrint(value)
}

