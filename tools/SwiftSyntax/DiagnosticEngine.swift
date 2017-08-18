//===------------ DiagnosticEngine.swift - Diagnostic Engine --------------===//
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
// This file provides the DiagnosticEngine, the class used to register
// and output diagnostics.
//===----------------------------------------------------------------------===//

import Foundation

/// The DiagnosticEngine allows Swift tools to emit diagnostics.
public class DiagnosticEngine {
  /// The list of consumers of the diagnostic passing through this engine.
  internal var consumers = [DiagnosticConsumer]()

  /// Adds the provided consumer to the consumers list.
  func addConsumer(_ consumer: DiagnosticConsumer) {
    consumers.append(consumer)
  }

  /// Registers the diagnostic with the engine.
  public func diagnose(_ diagnostic: Diagnostic) {
    for consumer in consumers {
      consumer.handle(diagnostic)
    }
  }

  /// Tells each consumer to finalize their diagnostic output.
  func finalize() {
    for consumer in consumers {
      consumer.finalize()
    }
  }
}
