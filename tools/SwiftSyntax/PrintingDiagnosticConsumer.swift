//===-- PrintingDiagnosticConsumer.swift - Printing Diagnsostic Consumer --===//
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
// This file provides a diagnostic consumer that formats and prints errors to
// stderr.
//===----------------------------------------------------------------------===//

import Foundation

/// PrintingDiagnosticConsumer formats diagnostics and prints them to the
/// console.
public class PrintingDiagnosticConsumer: DiagnosticConsumer {
  /// Creates a new PrintingDiagnosticConsumer.
  public init() {
  }

  /// Writes the text of the diagnostic to stderr.
  func write<T: CustomStringConvertible>(_ msg: T) {
    FileHandle.standardError.write("\(msg)".data(using: .utf8)!)
  }

  /// Prints the contents of a diagnostic to stderr.
  public func handle(_ diagnostic: Diagnostic) {
    write(diagnostic)
    for note in diagnostic.notes {
      write(note.asDiagnostic())
    }
  }

  /// Prints each of the fields in a diagnositic to stderr.
  public func write(_ diagnostic: Diagnostic) {
    if let loc = diagnostic.location {
      write("\(loc.file):\(loc.line):\(loc.column): ")
    } else {
      write("<unknown>:0:0: ")
    }
    switch diagnostic.message.severity {
    case .note: write("note: ")
    case .warning: write("warning: ")
    case .error: write("error: ")
    }
    write(diagnostic.message.text)
    write("\n")

    // TODO: Write original file contents out and highlight them.
  }

  public func finalize() {
    // Do nothing
  }
}
