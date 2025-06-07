//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2014-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation

/// A enum representing data types for legacy PropertyList type.
/// Note that the `identifier` enum is not strictly necessary,
/// but useful to semantically distinguish the strings that
/// represents object identifiers from those that are just data.
/// see: https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/PropertyLists/OldStylePlists/OldStylePLists.html
public enum PropertyList {
  case identifier(String)
  case string(String)
  case array([PropertyList])
  case dictionary([String: PropertyList])

  var string: String? {
    if case .string(let string) = self {
      return string
    }
    return nil
  }

  var array: [PropertyList]? {
    if case .array(let array) = self {
      return array
    }
    return nil
  }
}

extension PropertyList: CustomStringConvertible {
  public var description: String {
    String(decoding: serialize(), as: UTF8.self)
  }
}

extension PropertyList {
  /// Serializes the Plist enum.
  public func serialize() -> Data {
    var writer = UTF8Writer()
    writePlistRepresentation(to: &writer)
    return Data(writer.bytes)
  }
}

fileprivate extension PropertyList {
  struct UTF8Writer {
    private(set) var level: Int = 0
    private(set) var bytes: [UInt8] = []
    init() {
      self += "// !$*UTF8*$!\n"
    }

    mutating func withIndent(body: (inout Self) -> Void) {
      level += 1
      body(&self)
      level -= 1
    }

    mutating func append(_ byte: UInt8) {
      bytes.append(byte)
    }

    static func += (writer: inout UTF8Writer, str: StaticString) {
      str.withUTF8Buffer { utf8 in
        writer.bytes += utf8
      }
    }

    @_disfavoredOverload
    static func += (writer: inout UTF8Writer, str: String) {
      writer.bytes += str.utf8
    }

    mutating func indent() {
      for _ in 0 ..< level {
        self += "   "
      }
    }

    /// Appends the given string, with instances of quote (") and backward slash
    /// (\) characters escaped with a backslash.
    mutating func appendEscaped(_ string: String) {
      for char in string.utf8 {
        if char == UInt8(ascii: "\\") || char == UInt8(ascii: "\"") {
          append(UInt8(ascii: "\\"))
        }
        append(char)
      }
    }
  }

  /// Private function to generate OPENSTEP-style plist representation.
  func writePlistRepresentation(to writer: inout UTF8Writer) {
    // Do the appropriate thing for each type of plist node.
    switch self {
    case .identifier(let ident):
      // FIXME: we should assert that the identifier doesn't need quoting
      writer += ident

    case .string(let string):
      writer += "\""
      writer.appendEscaped(string)
      writer += "\""

    case .array(let array):
      writer += "(\n"
      writer.withIndent { writer in
        for (i, item) in array.enumerated() {
          writer.indent()
          item.writePlistRepresentation(to: &writer)
          writer += (i != array.count - 1) ? ",\n" : "\n"
        }
      }
      writer.indent()
      writer += ")"

    case .dictionary(let dict):
      let dict = dict.sorted(by: {
        // Make `isa` sort first (just for readability purposes).
        switch ($0.key, $1.key) {
        case ("isa", "isa"): return false
        case ("isa", _): return true
        case (_, "isa"): return false
        default: return $0.key < $1.key
        }
      })
      writer += "{\n"
      writer.withIndent { writer in
        for (key, value) in dict {
          writer.indent()
          writer += key
          writer += " = "
          value.writePlistRepresentation(to: &writer)
          writer += ";\n"
        }
      }
      writer.indent()
      writer += "}"
    }
  }
}
