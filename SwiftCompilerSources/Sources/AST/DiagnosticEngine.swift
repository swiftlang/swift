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

extension BridgedDiagnosticArgument {
  init(stringRef val: BridgedStringRef) {
    self.init(kind: .stringRef, value: .init(stringRefValue: val))
  }
  init(int val: Int) {
    self.init(kind: .int, value: .init(intValue: val))
  }
}

public protocol DiagnosticArgument {
  func _withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void)
}
extension String: DiagnosticArgument {
  public func _withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    withBridgedStringRef { fn(BridgedDiagnosticArgument(stringRef: $0)) }
  }
}
extension Int: DiagnosticArgument {
  public func _withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    fn(BridgedDiagnosticArgument(int: self))
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

  func withBridgedDiagnosticFixIt(_ fn: (BridgedDiagnosticFixIt) -> Void) {
    text.withBridgedStringRef { bridgedTextRef in
      let bridgedDiagnosticFixIt = BridgedDiagnosticFixIt(
        start: start.bridged,
        byteLength: byteLength,
        text: bridgedTextRef)
      fn(bridgedDiagnosticFixIt)
    }
  }
}

public struct DiagnosticEngine {
  private let bridged: BridgedDiagnosticEngine

  public init(bridged: BridgedDiagnosticEngine) {
    self.bridged = bridged
  }
  public init?(bridged: BridgedOptionalDiagnosticEngine) {
    guard let object = bridged.object else {
      return nil
    }
    self.bridged = BridgedDiagnosticEngine(object: object)
  }

  public func diagnose(_ position: SourceLoc?,
                       _ id: DiagID,
                       _ args: [DiagnosticArgument],
                       highlight: CharSourceRange? = nil,
                       fixIts: [DiagnosticFixIt] = []) {

    let bridgedSourceLoc: swift.SourceLoc = position.bridged
    let bridgedHighlightRange: BridgedCharSourceRange = highlight.bridged
    var bridgedArgs: [BridgedDiagnosticArgument] = []
    var bridgedFixIts: [BridgedDiagnosticFixIt] = []

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
