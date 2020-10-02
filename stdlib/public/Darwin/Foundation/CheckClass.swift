//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module
@_implementationOnly import _SwiftFoundationOverlayShims
import Dispatch

private let _queue = DispatchQueue(label: "com.apple.SwiftFoundation._checkClassAndWarnForKeyedArchivingQueue")
private var _seenClasses: Set<ObjectIdentifier> = []
private func _isClassFirstSeen(_ theClass: AnyClass) -> Bool {
  _queue.sync {
    let id = ObjectIdentifier(theClass)
    return _seenClasses.insert(id).inserted
  }
}

internal func _logRuntimeIssue(_ message: String) {
  NSLog("%@", message)
  _swift_reportToDebugger(0, message, nil)
}

extension NSKeyedUnarchiver {
  /// Checks if class `theClass` is good for archiving.
  ///
  /// If not, a runtime warning is printed.
  ///
  /// - Parameter operation: Specifies the archiving operation. Supported values
  ///     are 0 for archiving, and 1 for unarchiving.
  /// - Returns: 0 if the given class is safe to archive, and non-zero if it
  ///     isn't.
  @objc(_swift_checkClassAndWarnForKeyedArchiving:operation:)
  internal class func __swift_checkClassAndWarnForKeyedArchiving(
    _ theClass: AnyClass,
    operation: CInt
  ) -> CInt {
    if _swift_isObjCTypeNameSerializable(theClass) { return 0 }

    if _isClassFirstSeen(theClass) {
      let demangledName = String(reflecting: theClass)
      let mangledName = NSStringFromClass(theClass)

      let op = (operation == 1 ? "unarchive" : "archive")

      let message = """
        Attempting to \(op) Swift class '\(demangledName)' with unstable runtime name '\(mangledName)'.
        The runtime name for this class may change in the future, leading to non-decodable data.

        You can use the 'objc' attribute to ensure that the name will not change:
        "@objc(\(mangledName))"

        If there are no existing archives containing this class, you should choose a unique, prefixed name instead:
        "@objc(ABCMyModel)"
        """
      _logRuntimeIssue(message)
    }
    return 1
  }
}
