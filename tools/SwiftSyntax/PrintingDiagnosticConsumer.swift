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

public class PrintingDiagnosticConsumer: DiagnosticConsumer {
  func write<T: CustomStringConvertible>(_ msg: T) {
    FileHandle.standardError.write("\(msg)".data(using: .utf8)!)
  }

  func handle(_ diagnostic: Diagnostic) {
    write(diagnostic)
    for note in diagnostic.notes {
      write(note.asDiagnostic())
    }
  }

  func write(_ diagnostic: Diagnostic) {
    if let loc = diagnostic.location {
      write("\(loc.file):\(loc.line):\(loc.column): ")
    } else {
      write("<unknown>:0:0: ")
    }
    switch diagnostic.kind {
    case .note: write("note: ")
    case .warning: write("warning: ")
    case .error: write("error: ")
    }
    write(diagnostic.message)
    write("\n")

    // TODO: Write original file contents out and highlight them.
  }

  func finalize() {
    // Do nothing
  }
}
