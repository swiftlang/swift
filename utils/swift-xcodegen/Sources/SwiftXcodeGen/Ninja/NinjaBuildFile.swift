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
  var attributes: [Attribute.Key: Attribute]
  var buildRules: [BuildRule] = []

  init(
    attributes: [Attribute.Key: Attribute],
    buildRules: [BuildRule]
  ) {
    self.attributes = attributes
    self.buildRules = buildRules
  }
}

extension NinjaBuildFile {
  var buildConfiguration: BuildConfiguration? {
    attributes[.configuration]
      .flatMap { BuildConfiguration(rawValue: $0.value) }
  }
}

extension NinjaBuildFile {
  struct BuildRule: Hashable {
    let inputs: [String]
    let outputs: [String]
    let dependencies: [String]

    let attributes: [Attribute.Key: Attribute]
    private(set) var isPhony = false

    init(
      inputs: [String], outputs: [String], dependencies: [String],
      attributes: [Attribute.Key : Attribute]
    ) {
      self.inputs = inputs
      self.outputs = outputs
      self.dependencies = dependencies
      self.attributes = attributes
    }

    static func phony(for outputs: [String], inputs: [String]) -> Self {
      var rule = Self(
        inputs: inputs, outputs: outputs, dependencies: [], attributes: [:]
      )
      rule.isPhony = true
      return rule
    }
  }
}

extension NinjaBuildFile {
  struct Attribute: Hashable {
    var key: Key
    var value: String
  }
}

extension NinjaBuildFile: CustomDebugStringConvertible {
  var debugDescription: String {
    buildRules.map(\.debugDescription).joined(separator: "\n")
  }
}

extension NinjaBuildFile.BuildRule: CustomDebugStringConvertible {
  var debugDescription: String {
    """
    {
      inputs: \(inputs)
      outputs: \(outputs)
      dependencies: \(dependencies)
      attributes: \(attributes)
      isPhony: \(isPhony)
    }
    """
  }
}

extension NinjaBuildFile.Attribute: CustomStringConvertible {
  var description: String {
    "\(key.rawValue) = \(value)"
  }
}

extension NinjaBuildFile.Attribute {
  enum Key: String {
    case configuration = "CONFIGURATION"
    case defines = "DEFINES"
    case flags = "FLAGS"
    case includes = "INCLUDES"
    case swiftModule = "SWIFT_MODULE"
    case swiftModuleName = "SWIFT_MODULE_NAME"
    case swiftLibraryName = "SWIFT_LIBRARY_NAME"
    case swiftSources = "SWIFT_SOURCES"
    case command = "COMMAND"
  }
}
