//===--- JSONDiagnosticConsumer.swift - JSON Serializer for Diagnostics ---===//
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
// This file provides a JSON serializer for compiler diagnostics emitted using
// the DiagnosticEngine.
//===----------------------------------------------------------------------===//

import Foundation

public final class JSONDiagnosticConsumer: DiagnosticConsumer {
  /// Enumerates the possible places this consumer might output diagnostics.
  private enum OutputFormat {
    case url(URL)
    case stdout
  }

  /// The current set of diagnostics that have been emitted.
  private var diagnostics = [Diagnostic]()

  /// The output format this consumer will output the generated JSON to.
  private let outputFormat: OutputFormat

  /// Creates a JSONDiagnosticConsumer that will output the generated JSON
  /// to a file at the given URL.
  /// - parameter outputURL: The URL that the consumer will write JSON
  ///                        diagnostics to.
  /// - note: If a file exists at the URL already, it will be overwritten.
  public init(outputURL: URL) {
    self.outputFormat = .url(outputURL)
  }

  /// Creates a JSONDiagnosticConsumer that will output the generated JSON
  /// to standard output.
  public init() {
    self.outputFormat = .stdout
  }

  /// Adds the diagnostic to the list of registered diagnostics.
  public func handle(_ diagnostic: Diagnostic) {
    self.diagnostics.append(diagnostic)
  }

  /// Writes the contents of the diagnostics as JSON out to the specified
  /// output location.
  public func finalize() {
    do {
      let encoder = JSONEncoder()
      encoder.outputFormatting = .prettyPrinted
      let data = try encoder.encode(diagnostics)
      switch outputFormat {
      case .url(let url):
        if FileManager.default.fileExists(atPath: url.path) {
          try FileManager.default.removeItem(at: url)
        }
        FileManager.default.createFile(atPath: url.path, contents: data)
      case .stdout:
        FileHandle.standardOutput.write(data)
      }
    } catch {
      let dummyEngine = DiagnosticEngine()
      let printingConsumer = PrintingDiagnosticConsumer()
      dummyEngine.addConsumer(printingConsumer)
      dummyEngine.diagnose(.couldNotOutputJSON)
    }
  }
}

extension Diagnostic.Message {
    static let couldNotOutputJSON =
        Diagnostic.Message(.error, "failed emitting JSON diagnostics")
}