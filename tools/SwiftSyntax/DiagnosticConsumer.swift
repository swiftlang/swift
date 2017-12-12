//===---------- DiagnosticConsumer.swift - Diagnostic Consumer ------------===//
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
// This file provides the DiagnosticConsumer protocol.
//===----------------------------------------------------------------------===//

/// An object that intends to receive notifications when diagnostics are
/// emitted.
public protocol DiagnosticConsumer {
  /// Handle the provided diagnostic which has just been registered with the
  /// DiagnosticEngine.
  func handle(_ diagnostic: Diagnostic)

  /// Finalize the consumption of diagnostics, flushing to disk if necessary.
  func finalize()
}
