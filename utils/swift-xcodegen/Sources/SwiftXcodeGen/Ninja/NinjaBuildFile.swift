//===--- NinjaBuildFile.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

struct NinjaBuildFile {
  var bindings: Bindings
  var rules: [String: Rule]
  var buildEdges: [BuildEdge] = []

  init(
    bindings: [String: String],
    rules: [String: Rule],
    buildEdges: [BuildEdge]
  ) {
    self.bindings = Bindings(storage: bindings)
    self.buildEdges = buildEdges
    self.rules = rules
  }
}

extension NinjaBuildFile {
  var buildConfiguration: BuildConfiguration? {
    bindings[.configuration]
      .flatMap { BuildConfiguration(rawValue: $0) }
  }
}

extension NinjaBuildFile {

  struct Bindings: Hashable {
    let values: [String: String]

    init(storage: [String : String]) {
      self.values = storage
    }

    subscript(key: String) -> String? {
      values[key]
    }
  }

  struct Rule: Equatable {
    let name: String
    var bindings: Bindings

    init(name: String, bindings: [String: String]) {
      self.name = name
      self.bindings = Bindings(storage: bindings)
    }
  }

  struct BuildEdge: Hashable {
    let ruleName: String
    let inputs: [String]
    let outputs: [String]
    let dependencies: [String]
    var bindings: Bindings

    var isPhony: Bool {
      ruleName == "phony"
    }

    init(
      ruleName: String,
      inputs: [String], outputs: [String], dependencies: [String],
      bindings: [String: String]
    ) {
      self.ruleName = ruleName
      self.inputs = inputs
      self.outputs = outputs
      self.dependencies = dependencies
      self.bindings = Bindings(storage: bindings)
    }

    static func phony(for outputs: [String], inputs: [String]) -> Self {
      return Self(
        ruleName: "phony", inputs: inputs, outputs: outputs, dependencies: [], bindings: [:]
      )
    }
  }
}


fileprivate enum NinjaCommandLineError: Error {
  case unknownRule(String)
  case missingCommandBinding
}

extension NinjaBuildFile {

  func commandLine(for edge: BuildEdge) throws -> String {
    guard let rule = self.rules[edge.ruleName] else {
      throw NinjaCommandLineError.unknownRule(edge.ruleName)
    }

    // Helper to get a substitution value for ${key}.
    // Note that we don't do built-in substitutions (e.g. $in, $out) for now.
    func value(for key: String) -> String? {
      edge.bindings[key] ?? rule.bindings[key] ?? self.bindings[key]
    }

    func eval(string: String) -> String {
      var result = ""
      string.scanningUTF8 { scanner in
        while scanner.hasInput {
          if let prefix = scanner.eat(while: { $0 != "$" }) {
            result += String(utf8: prefix)
          }
          guard scanner.tryEat("$") else {
            // Reached the end.
            break
          }

          let substituted: String? = scanner.tryEating { scanner in
            // Parse the variable name.
            let key: String
            if scanner.tryEat("{"), let keyName = scanner.eat(while: { $0 != "}" }), scanner.tryEat("}") {
              key = String(utf8: keyName)
            } else if let keyName = scanner.eat(while: { $0.isNinjaVarName }) {
              key = String(utf8: keyName)
            } else {
              return nil
            }

            return value(for: key)
          }

          if let substituted {
            // Recursive substitutions.
            result += eval(string: substituted)
          } else {
            // Was not a variable, restore '$' and move on.
            result += "$"
          }
        }
      }
      return result
    }

    guard let commandLine = rule.bindings["command"] else {
      throw NinjaCommandLineError.missingCommandBinding
    }
    return eval(string: commandLine)
  }
}

extension Byte {
  fileprivate var isNinjaVarName: Bool {
    switch self {
    case "0"..."9", "a"..."z", "A"..."Z", "_", "-":
      return true
    default:
      return false
    }
  }
}

extension NinjaBuildFile: CustomDebugStringConvertible {
  var debugDescription: String {
    buildEdges.map(\.debugDescription).joined(separator: "\n")
  }
}

extension NinjaBuildFile.BuildEdge: CustomDebugStringConvertible {
  var debugDescription: String {
    """
    {
      inputs: \(inputs)
      outputs: \(outputs)
      dependencies: \(dependencies)
      bindings: \(bindings)
      isPhony: \(isPhony)
    }
    """
  }
}

extension NinjaBuildFile.Bindings {
  enum Key: String {
    case configuration = "CONFIGURATION"
    case defines = "DEFINES"
    case flags = "FLAGS"
    case includes = "INCLUDES"
    case swiftModule = "SWIFT_MODULE"
    case swiftModuleName = "SWIFT_MODULE_NAME"
    case swiftLibraryName = "SWIFT_LIBRARY_NAME"
    case swiftSources = "SWIFT_SOURCES"
  }

  subscript(key: Key) -> String? {
    return self[key.rawValue]
  }
}
