//===--------------- Diagnostics.swift - Diagnostic Emitter ---------------===//
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
// This file provides the Diagnostic, Note, and FixIt types.
//===----------------------------------------------------------------------===//

/// Represents a source location in a Swift file.
public struct SourceLocation {
  /// The line in the file where this location resides.
  public let line: Int

  /// The UTF-8 byte offset from the beginning of the line where this location
  /// resides.
  public let column: Int

  /// The UTF-8 byte offset into the file where this location resides.
  public let offset: Int

  /// The file in which this location resides.
  public let file: String
}

/// Represents a start and end location in a Swift file.
public struct SourceRange {
  /// The beginning location in the source range.
  public let start: SourceLocation

  /// The ending location in the source range.
  public let end: SourceLocation
}

/// A FixIt represents a change to source code in order to "correct" a
/// diagnostic.
public enum FixIt {
  /// Remove the characters from the source file over the provided source range.
  case remove(SourceRange)

  /// Insert, at the provided source location, the provided string.
  case insert(SourceLocation, String)

  /// Replace the characters at the provided source range with the provided
  /// string.
  case replace(SourceRange, String)

  /// The source range associated with a FixIt. If this is an insertion,
  /// it is a range with the same start and end location.
  var range: SourceRange {
    switch self {
    case .remove(let range), .replace(let range, _): return range
    case .insert(let loc, _): return SourceRange(start: loc, end: loc)
    }
  }

  /// The text associated with this FixIt. If this is a removal, the text is
  /// the empty string.
  var text: String {
    switch self {
    case .remove(_): return ""
    case .insert(_, let text), .replace(_, let text): return text
    }
  }
}

/// A Note attached to a Diagnostic. This provides more context for a specific
/// error, and optionally allows for FixIts.
public struct Note {
  /// The note's message.
  public let message: String

  /// The source location where the note should point.
  public let location: SourceLocation?

  /// An array of source ranges that should be highlighted.
  public let highlights: [SourceRange]

  /// An array of FixIts that apply to this note.
  public let fixIts: [FixIt]

  /// Creates a Note with the provided message, location, highlights, and
  /// FixIts.
  public init(_ message: String, location: SourceLocation?,
              highlights: [SourceRange] = [], fixIts: [FixIt] = []) {
    self.message = message
    self.location = location
    self.highlights = highlights
    self.fixIts = fixIts
  }

  /// Converts this Note to a Diagnostic for serialization.
  func asDiagnostic() -> Diagnostic {
    return Diagnostic(kind: .note, message: message, location: location,
                      highlights: highlights, fixIts: fixIts, notes: [])
  }
}

/// A Diagnostic message that can be emitted regarding some piece of code.
public struct Diagnostic {
  // These values must match clang/Frontend/SerializedDiagnostics.h
  /// The severity of the diagnostic.
  public enum Kind: UInt8 {
    case note = 1
    case warning = 2
    case error = 3
  }
  /// The Kind of diagnostic. This can be note, error, or warning.
  public let kind: Kind

  /// The diagnostic's message.
  public let message: String

  /// The location the diagnostic should point.
  public let location: SourceLocation?

  /// An array of source ranges to highlight.
  public let highlights: [SourceRange]

  /// An array of possible FixIts to apply to this diagnostic.
  public let fixIts: [FixIt]

  /// An array of notes providing more context for this diagnostic.
  public let notes: [Note]

  /// Creates an `error` diagnostic.
  /// - Parameters:
  ///   - message: The message for the diagnostic.
  ///   - location: The location where the diagnostic should point, if any.
  ///   - highlights: Ranges of code that should be highlighted in the resulting
  ///                 diagnostic.
  ///   - fixIts: Possible fixes for the error in the diagnostic.
  public static func error(_ message: String, location: SourceLocation?,
                           highlights: [SourceRange] = [], fixIts: [FixIt] = [],
                           notes: [Note] = []) -> Diagnostic {
    return Diagnostic(kind: .error, message: message, location: location,
                      highlights: highlights, fixIts: fixIts, notes: notes)
  }

  /// Creates a `warning` diagnostic.
  /// - Parameters:
  ///   - message: The message for the diagnostic.
  ///   - location: The location where the diagnostic should point, if any.
  ///   - highlights: Ranges of code that should be highlighted in the resulting
  ///                 diagnostic.
  ///   - fixIts: Possible fixes for the warning in the diagnostic.
  public static func warning(_ message: String, location: SourceLocation?,
                             highlights: [SourceRange] = [],
                             fixIts: [FixIt] = [],
                             notes: [Note] = []) -> Diagnostic {
    return Diagnostic(kind: .warning, message: message, location: location,
                      highlights: highlights, fixIts: fixIts, notes: notes)
  }
}
