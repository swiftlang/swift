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

@_exported import ASTBridging
import Basic

public typealias DiagID = BridgedDiagID

extension BridgedDiagnosticArgument {
  init(stringRef val: BridgedStringRef) {
    self.init(kind: .stringRef, value: .init(stringRefValue: val))
  }
  init(int val: Int) {
    self.init(kind: .int, value: .init(intValue: val))
  }
  init(uInt val: UInt) {
    self.init(kind: .uInt, value: .init(uintValue: val))
  }
}

public protocol DiagnosticArgument {
  func withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void)
}
extension String: DiagnosticArgument {
  public func withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    withBridgedStringRef { fn(BridgedDiagnosticArgument(stringRef: $0)) }
  }
}
extension Int: DiagnosticArgument {
  public func withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    fn(BridgedDiagnosticArgument(int: self))
  }
}
extension UInt: DiagnosticArgument {
  public func withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    fn(BridgedDiagnosticArgument(uInt: self))
  }
}

public struct DiagnosticFixIt {
  public var start: SourceLoc
  public var byteLength: Int
  public var text: String

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
  private var bridged: BridgedDiagnosticEngine

  public init(bridged: BridgedDiagnosticEngine) {
    self.bridged = bridged
  }

  public func diagnose(_ position: SourceLoc?,
                       _ id: DiagID,
                       _ args: [DiagnosticArgument],
                       highlight: CharSourceRange? = nil,
                       fixIts: [DiagnosticFixIt] = []) {

    let bridgedSourceLoc: BridgedSourceLoc = position.bridged
    let bridgedHighlightRange: BridgedCharSourceRange = highlight.bridged
    var bridgedArgs: [BridgedDiagnosticArgument] = []
    var bridgedFixIts: [BridgedDiagnosticFixIt] = []

    var closure: () -> Void = {
      bridgedArgs.withBridgedArrayRef { bridgedArgsRef in
        bridgedFixIts.withBridgedArrayRef { bridgedFixItsRef in
          DiagnosticEngine_diagnose(bridged, bridgedSourceLoc,
                                    id, bridgedArgsRef,
                                    bridgedHighlightRange, bridgedFixItsRef)
        }
      }
    }
    for arg in args {
      closure = { [closure, arg] in
        arg.withBridgedDiagnosticArgument { bridgedArg in
          bridgedArgs.append(bridgedArg)
          closure()
        }
      }
    }
    for fixIt in fixIts {
      closure = { [closure, fixIt] in
        fixIt.withBridgedDiagnosticFixIt { bridgedFixIt in
          bridgedFixIts.append(bridgedFixIt)
          closure()
        }
      }
    }

    closure()
  }
}
