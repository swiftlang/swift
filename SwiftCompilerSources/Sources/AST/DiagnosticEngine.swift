//===--- DiagnosticEngine.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging

import Basic

public typealias DiagID = BridgedDiagID

public protocol DiagnosticArgument {
  func _withBridgedDiagnosticArgument(_ fn: (swift.DiagnosticArgument) -> Void)
}
extension String: DiagnosticArgument {
  public func _withBridgedDiagnosticArgument(_ fn: (swift.DiagnosticArgument) -> Void) {
    withStringRef { fn(swift.DiagnosticArgument($0)) }
  }
}
extension Int: DiagnosticArgument {
  public func _withBridgedDiagnosticArgument(_ fn: (swift.DiagnosticArgument) -> Void) {
    fn(swift.DiagnosticArgument(Int32(self)))
  }
}

public struct DiagnosticFixIt {
  public let start: SourceLoc
  public let byteLength: Int
  public let text: String

  public init(start: SourceLoc, byteLength: Int, replacement text: String) {
    self.start = start
    self.byteLength = byteLength
    self.text = text
  }

  func withBridgedDiagnosticFixIt(_ fn: (swift.DiagnosticInfo.FixIt) -> Void) {
    text.withStringRef { bridgedTextRef in
      let bridgedDiagnosticFixIt = swift.DiagnosticInfo.FixIt(
        swift.CharSourceRange(start.bridged, UInt32(byteLength)),
        bridgedTextRef,
        llvm.ArrayRef<swift.DiagnosticArgument>())
      fn(bridgedDiagnosticFixIt)
    }
  }
}

public struct DiagnosticEngine {
  private let bridged: swift.DiagnosticEngine

  public init(bridged: swift.DiagnosticEngine) {
    self.bridged = bridged
  }

  public func diagnose(_ position: SourceLoc?,
                       _ id: DiagID,
                       _ args: [DiagnosticArgument],
                       highlight: CharSourceRange? = nil,
                       fixIts: [DiagnosticFixIt] = []) {

    let bridgedSourceLoc: swift.SourceLoc = position.bridged
    let bridgedHighlightRange: swift.CharSourceRange = highlight.bridged
    var bridgedArgs: [swift.DiagnosticArgument] = []
    var bridgedFixIts: [swift.DiagnosticInfo.FixIt] = []

    // Build a higher-order function to wrap every 'withBridgedXXX { ... }'
    // calls, so we don't escape anything from the closure. 'bridgedArgs' and
    // 'bridgedFixIts' are temporary storage to store bridged values. So they
    // should not be used after the closure is executed.
 
    var closure: () -> Void = {
      bridgedArgs.withBridgedArrayRef { bridgedArgsRef in
        bridgedFixIts.withBridgedArrayRef { bridgedFixItsRef in
          DiagnosticEngine_diagnose(bridged, bridgedSourceLoc,
                                    id, bridgedArgsRef,
                                    bridgedHighlightRange, bridgedFixItsRef)
        }
      }
    }
    // 'reversed()' because the closure should be wrapped in that order.
    for arg in args.reversed() {
      closure = { [closure, arg] in
        arg._withBridgedDiagnosticArgument { bridgedArg in
          bridgedArgs.append(bridgedArg)
          closure()
        }
      }
    }
    // 'reversed()' because the closure should be wrapped in that order.
    for fixIt in fixIts.reversed() {
      closure = { [closure, fixIt] in
        fixIt.withBridgedDiagnosticFixIt { bridgedFixIt in
          bridgedFixIts.append(bridgedFixIt)
          closure()
        }
      }
    }

    closure()
  }

  public func diagnose(_ position: SourceLoc?,
                       _ id: DiagID,
                       _ args: DiagnosticArgument...,
                       highlight: CharSourceRange? = nil,
                       fixIts: DiagnosticFixIt...) {
    diagnose(position, id, args, highlight: highlight, fixIts: fixIts)
  }
}
