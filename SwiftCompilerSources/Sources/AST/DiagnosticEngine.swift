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
  func _withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void)
}
extension String: DiagnosticArgument {
  public func _withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    _withBridgedStringRef { fn(BridgedDiagnosticArgument($0)) }
  }
}
extension StringRef: DiagnosticArgument {
  public func _withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    fn(BridgedDiagnosticArgument(_bridged))
  }
}
extension Int: DiagnosticArgument {
  public func _withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    fn(BridgedDiagnosticArgument(self))
  }
}
extension Type: DiagnosticArgument {
  public func _withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    fn(bridged.asDiagnosticArgument())
  }
}

public struct DiagnosticFixIt {
  let start: SourceLoc
  let byteLength: Int
  let text: String

  init(start: SourceLoc, byteLength: Int, replacement text: String) {
    self.start = start
    self.byteLength = byteLength
    self.text = text
  }

  func withBridgedDiagnosticFixIt(_ fn: (BridgedDiagnosticFixIt) -> Void) {
    text._withBridgedStringRef { bridgedTextRef in
      let bridgedDiagnosticFixIt = BridgedDiagnosticFixIt(
        start.bridged, UInt32(byteLength),
        bridgedTextRef)
      fn(bridgedDiagnosticFixIt)
    }
  }
}

public struct DiagnosticEngine {
  private let bridged: BridgedDiagnosticEngine

  public init(bridged: BridgedDiagnosticEngine) {
    self.bridged = bridged
  }
  init?(bridged: BridgedNullableDiagnosticEngine) {
    guard let raw = bridged.raw else {
      return nil
    }
    self.bridged = BridgedDiagnosticEngine(raw: raw)
  }

  public func diagnose(_ id: DiagID,
                       _ args: [DiagnosticArgument],
                       at position: SourceLoc?,
                       highlight: CharSourceRange? = nil,
                       fixIts: [DiagnosticFixIt] = []) {

    let bridgedSourceLoc: BridgedSourceLoc = position.bridged
    let highlightStart: BridgedSourceLoc
    let highlightLength: UInt32
    if let highlight = highlight {
      highlightStart = highlight.start.bridged
      highlightLength = highlight.byteLength
    } else {
      highlightStart = BridgedSourceLoc()
      highlightLength = 0
    }
    var bridgedArgs: [BridgedDiagnosticArgument] = []
    var bridgedFixIts: [BridgedDiagnosticFixIt] = []

    // Build a higher-order function to wrap every 'withBridgedXXX { ... }'
    // calls, so we don't escape anything from the closure. 'bridgedArgs' and
    // 'bridgedFixIts' are temporary storage to store bridged values. So they
    // should not be used after the closure is executed.
 
    var closure: () -> Void = {
      bridgedArgs.withBridgedArrayRef { bridgedArgsRef in
        bridgedFixIts.withBridgedArrayRef { bridgedFixItsRef in
          bridged.diagnose(at: bridgedSourceLoc, id, bridgedArgsRef,
                           highlightAt: highlightStart,
                           highlightLength: highlightLength,
                           fixIts: bridgedFixItsRef)
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

  public func diagnose(_ id: DiagID,
                       _ args: DiagnosticArgument...,
                       at position: SourceLoc?,
                       highlight: CharSourceRange? = nil,
                       fixIts: DiagnosticFixIt...) {
    diagnose(id, args, at: position, highlight: highlight, fixIts: fixIts)
  }

  public func diagnose<SourceLocation: ProvidingSourceLocation>(_ diagnostic: Diagnostic<SourceLocation>) {
    let loc = diagnostic.location.getSourceLocation(diagnosticEngine: self)
    diagnose(diagnostic.id, diagnostic.arguments, at: loc)
  }

  /// Loads the file at `path` and returns a `SourceLoc` pointing to `line` and `column` in the file.
  /// Returns nil if the file cannot be loaded.
  public func getLocationFromExternalSource(path: StringRef, line: Int, column: Int) -> SourceLoc? {
    return SourceLoc(bridged: bridged.getLocationFromExternalSource(path: path._bridged, line: line, column: column))
  }
}

/// Something which can provide a `SourceLoc` for diagnostics.
public protocol ProvidingSourceLocation {
  func getSourceLocation(diagnosticEngine: DiagnosticEngine) -> SourceLoc?
}

extension SourceLoc: ProvidingSourceLocation {
  public func getSourceLocation(diagnosticEngine: DiagnosticEngine) -> SourceLoc? { self }
}

/// A utility struct which allows throwing a Diagnostic.
public struct Diagnostic<SourceLocation: ProvidingSourceLocation> : Error {
  public let id: DiagID
  public let arguments: [DiagnosticArgument]
  public let location: SourceLocation

  public init(_ id: DiagID, _ arguments: DiagnosticArgument..., at location: SourceLocation) {
    self.init(id, arguments, at: location)
  }

  public init(_ id: DiagID, _ arguments: [DiagnosticArgument], at location: SourceLocation) {
    self.id = id
    self.arguments = arguments
    self.location = location
  }
}
