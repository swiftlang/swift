//===--- DiagnoseicEngine.swift -------------------------------------------===//
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

extension BridgedDiagnoseicArgument {
  init(stringRef val: BridgedStringRef) {
    self.init(kind: .stringRef, value: .init(stringRefValue: val))
  }
  init(int val: Int) {
    self.init(kind: .int, value: .init(intValue: val))
  }
}

public protocol DiagnoseicArgument {
  func _withBridgedDiagnoseicArgument(_ fn: (BridgedDiagnoseicArgument) -> Void)
}
extension String: DiagnoseicArgument {
  public func _withBridgedDiagnoseicArgument(_ fn: (BridgedDiagnoseicArgument) -> Void) {
    withBridgedStringRef { fn(BridgedDiagnoseicArgument(stringRef: $0)) }
  }
}
extension Int: DiagnoseicArgument {
  public func _withBridgedDiagnoseicArgument(_ fn: (BridgedDiagnoseicArgument) -> Void) {
    fn(BridgedDiagnoseicArgument(int: self))
  }
}

public struct DiagnoseicFixIt {
  public let start: SourceLoc
  public let byteLength: Int
  public let text: String

  public init(start: SourceLoc, byteLength: Int, replacement text: String) {
    self.start = start
    self.byteLength = byteLength
    self.text = text
  }

  func withBridgedDiagnoseicFixIt(_ fn: (BridgedDiagnoseicFixIt) -> Void) {
    text.withBridgedStringRef { bridgedTextRef in
      let bridgedDiagnoseicFixIt = BridgedDiagnoseicFixIt(
        start: start.bridged,
        byteLength: byteLength,
        text: bridgedTextRef)
      fn(bridgedDiagnoseicFixIt)
    }
  }
}

public struct DiagnoseicEngine {
  private let bridged: BridgedDiagnoseicEngine

  public init(bridged: BridgedDiagnoseicEngine) {
    self.bridged = bridged
  }

  public func diagnose(_ position: SourceLoc?,
                       _ id: DiagID,
                       _ args: [DiagnoseicArgument],
                       highlight: CharSourceRange? = nil,
                       fixIts: [DiagnoseicFixIt] = []) {

    let bridgedSourceLoc: BridgedSourceLoc = position.bridged
    let bridgedHighlightRange: BridgedCharSourceRange = highlight.bridged
    var bridgedArgs: [BridgedDiagnoseicArgument] = []
    var bridgedFixIts: [BridgedDiagnoseicFixIt] = []

    // Build a higher-order function to wrap every 'withBridgedXXX { ... }'
    // calls, so we don't escape anything from the closure. 'bridgedArgs' and
    // 'bridgedFixIts' are temporary storage to store bridged values. So they
    // should not be used after the closure is executed.
 
    var closure: () -> Void = {
      bridgedArgs.withBridgedArrayRef { bridgedArgsRef in
        bridgedFixIts.withBridgedArrayRef { bridgedFixItsRef in
          DiagnoseicEngine_diagnose(bridged, bridgedSourceLoc,
                                    id, bridgedArgsRef,
                                    bridgedHighlightRange, bridgedFixItsRef)
        }
      }
    }
    // 'reversed()' because the closure should be wrapped in that order.
    for arg in args.reversed() {
      closure = { [closure, arg] in
        arg._withBridgedDiagnoseicArgument { bridgedArg in
          bridgedArgs.append(bridgedArg)
          closure()
        }
      }
    }
    // 'reversed()' because the closure should be wrapped in that order.
    for fixIt in fixIts.reversed() {
      closure = { [closure, fixIt] in
        fixIt.withBridgedDiagnoseicFixIt { bridgedFixIt in
          bridgedFixIts.append(bridgedFixIt)
          closure()
        }
      }
    }

    closure()
  }

  public func diagnose(_ position: SourceLoc?,
                       _ id: DiagID,
                       _ args: DiagnoseicArgument...,
                       highlight: CharSourceRange? = nil,
                       fixIts: DiagnoseicFixIt...) {
    diagnose(position, id, args, highlight: highlight, fixIts: fixIts)
  }
}
