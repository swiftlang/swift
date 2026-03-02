//===----------------------------------------------------------------------===//
//
// SARIFSchema.swift
// SARIF (Static Analysis Results Interchange Format) Version 2.1.0
//
// This file contains Swift structs representing the SARIF v2.1.0 format
// as defined by the OASIS standard.
//
//===----------------------------------------------------------------------===//

import Foundation

/// The top-level object for a SARIF log file.
public struct SARIFLog: Codable, Sendable {
  public let version: String = "2.1.0"
  public let schema: String = "https://json.schemastore.org/sarif-2.1.0.json"
  public let runs: [Run]

  enum CodingKeys: String, CodingKey {
    case version
    case schema = "$schema"
    case runs
  }

  public init(
    runs: [Run]
  ) {
    self.runs = runs
  }

  public func toJSONData(
    with outputFormatting: JSONEncoder.OutputFormatting = [.prettyPrinted, .sortedKeys, .withoutEscapingSlashes]
  ) throws -> Data {
    let encoder = JSONEncoder()
    encoder.outputFormatting = outputFormatting
    let jsonData = try encoder.encode(self)
    return jsonData
  }

  public func toJSONString() throws -> String {
    let jsonData = try toJSONData()
    // JSONEncoder produces valid UTF-8 encoded data by default
    return String(data: jsonData, encoding: .utf8)!
  }

  public func toJSONFile(url: URL, options: Data.WritingOptions = []) throws {
    let jsonData = try toJSONData()
    try jsonData.write(to: url, options: options)
  }
}

/// Describes a single run of an analysis tool and contains the output of that run.
public struct Run: Codable, Sendable {
  public let tool: Tool
  public let invocations: [Invocation]
  public let results: [Result]
  public let artifacts: [Artifact]

  public init(
    tool: Tool,
    invocations: [Invocation],
    results: [Result],
    artifacts: [Artifact]
  ) {
    self.tool = tool
    self.invocations = invocations
    self.results = results
    self.artifacts = artifacts
  }
}

/// Describes the analysis tool that was run.
public struct Tool: Codable, Sendable {
  public let driver: ToolComponent

  public init(driver: ToolComponent) {
    self.driver = driver
  }
}

/// A component of the analysis tool, such as the driver or an extension.
public struct ToolComponent: Codable, Sendable {
  public let name: String
  public let version: String
  public let informationUri: String
  public let organization: String

  public init(
    name: String,
    version: String,
    informationUri: String,
    organization: String,
  ) {
    self.name = name
    self.version = version
    self.informationUri = informationUri
    self.organization = organization
  }
}

/// A result produced by an analysis tool.
public struct Result: Codable, Sendable {
  public let ruleId: String
  public let message: Message
  public let level: Level
  public let kind: Kind
  public let locations: [Location]
  public let fixes: [Fix]?
  public let relatedLocations: [Location]?

  public init(
    ruleId: String,
    message: Message,
    level: Level,
    kind: Kind,
    locations: [Location],
    fixes: [Fix]? = nil,
    relatedLocations: [Location]? = nil
  ) {
    self.ruleId = ruleId
    self.message = message
    self.level = level
    self.kind = kind
    self.locations = locations
    self.fixes = fixes
    self.relatedLocations = relatedLocations
  }

  /// The level of a result.
  public enum Level: String, Codable, Sendable {
    case none
    case note
    case warning
    case error
  }

  /// The kind of result.
  public enum Kind: String, Codable, Sendable {
    case notApplicable
    case pass
    case fail
    case review
    case `open`
    case informational
  }
}

/// A location within a programming artifact.
public struct Location: Codable, Sendable {
  public let physicalLocation: PhysicalLocation
  public let logicalLocations: [LogicalLocation]?
  public let message: Message?

  public init(
    physicalLocation: PhysicalLocation,
    logicalLocations: [LogicalLocation]? = nil,
    message: Message? = nil
  ) {
    self.physicalLocation = physicalLocation
    self.logicalLocations = logicalLocations
    self.message = message
  }
}

/// A logical location such as a function, class, or namespace.
public struct LogicalLocation: Codable, Sendable {
  public let name: String
  public let fullyQualifiedName: String
  public let kind: String
  public let parentIndex: Int?

  public init(
    name: String,
    fullyQualifiedName: String,
    kind: String,
    parentIndex: Int? = nil
  ) {
    self.name = name
    self.fullyQualifiedName = fullyQualifiedName
    self.kind = kind
    self.parentIndex = parentIndex
  }
}

/// A physical location relevant to a result.
public struct PhysicalLocation: Codable, Sendable {
  public let artifactLocation: ArtifactLocation
  public let region: Region

  public init(
    artifactLocation: ArtifactLocation,
    region: Region,
  ) {
    self.artifactLocation = artifactLocation
    self.region = region
  }
}

/// Specifies the location of an artifact.
public struct ArtifactLocation: Codable, Sendable {
  public let uri: String?
  public let index: Int?

  public init(uri: String? = nil, index: Int? = nil) {
    self.uri = uri
    self.index = index
  }
}

/// A region within an artifact where a result was detected.
public struct Region: Codable, Sendable {
  public let startLine: Int
  public let startColumn: Int
  public let endLine: Int?
  public let endColumn: Int?
  public let snippet: ArtifactContent?

  public init(
    startLine: Int,
    startColumn: Int,
    endLine: Int? = nil,
    endColumn: Int? = nil,
    snippet: ArtifactContent? = nil
  ) {
    self.startLine = startLine
    self.startColumn = startColumn
    self.endLine = endLine
    self.endColumn = endColumn
    self.snippet = snippet
  }
}

/// Encapsulates a message intended to be read by the end user.
public struct Message: Codable, Sendable {
  public let text: String
  public let id: String?
  public let arguments: [String]?

  public init(
    text: String,
    id: String? = nil,
    arguments: [String]? = nil
  ) {
    self.text = text
    self.id = id
    self.arguments = arguments
  }
}

/// A single artifact.
public struct Artifact: Codable, Sendable {
  public let location: ArtifactLocation
  public let mimeType: String
  public let sourceLanguage: String
  public let length: Int?
  public let contents: ArtifactContent?

  public init(
    location: ArtifactLocation,
    mimeType: String,
    sourceLanguage: String,
    length: Int? = nil,
    contents: ArtifactContent? = nil
  ) {
    self.location = location
    self.mimeType = mimeType
    self.sourceLanguage = sourceLanguage
    self.length = length
    self.contents = contents

  }
}

/// Represents the contents of an artifact.
public struct ArtifactContent: Codable, Sendable {
  public let text: String?

  public init(text: String) {
    self.text = text
  }
}

/// A proposed fix for a problem indicated by a result.
public struct Fix: Codable, Sendable {
  public let description: Message
  public let artifactChanges: [ArtifactChange]

  public init(description: Message, artifactChanges: [ArtifactChange]) {
    self.description = description
    self.artifactChanges = artifactChanges
  }
}

/// A change to a single artifact.
public struct ArtifactChange: Codable, Sendable {
  public let artifactLocation: ArtifactLocation
  public let replacements: [Replacement]

  public init(artifactLocation: ArtifactLocation, replacements: [Replacement]) {
    self.artifactLocation = artifactLocation
    self.replacements = replacements
  }
}

/// The replacement of a single region of an artifact.
public struct Replacement: Codable, Sendable {
  public let deletedRegion: Region
  public let insertedContent: ArtifactContent?

  public init(deletedRegion: Region, insertedContent: ArtifactContent? = nil) {
    self.deletedRegion = deletedRegion
    self.insertedContent = insertedContent
  }
}

/// Describes the invocation of the analysis tool.
public struct Invocation: Codable, Sendable {
  public let executableLocation: ArtifactLocation?
  public let arguments: [String]?
  public let commandLine: String?
  public let properties: PropertyBag?

  public init(
    executableLocation: ArtifactLocation? = nil,
    arguments: [String]? = nil,
    commandLine: String? = nil,
    properties: PropertyBag? = nil
  ) {
    self.executableLocation = executableLocation
    self.arguments = arguments
    self.commandLine = commandLine
    self.properties = properties
  }
}

public struct PropertyBag: Codable, Sendable {
  private var storage: [String: PropertyValue]

  public init(_ values: [String: PropertyValue] = [:]) {
    self.storage = values
  }

  public subscript(key: String) -> PropertyValue? {
    get { storage[key] }
    set { storage[key] = newValue }
  }

  public var isEmpty: Bool {
    storage.isEmpty
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: DynamicCodingKey.self)
    for (key, value) in storage.sorted(by: { $0.key < $1.key }) {
      let codingKey = DynamicCodingKey(stringValue: key)
      try value.encode(to: &container, forKey: codingKey)
    }
  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: DynamicCodingKey.self)
    var result: [String: PropertyValue] = [:]
    for key in container.allKeys {
      if let value = try? PropertyValue(from: decoder, container: container, key: key) {
        result[key.stringValue] = value
      }
    }
    self.storage = result
  }
}

public enum PropertyValue: Sendable {
  case string(String)
  case int(Int)
  case bool(Bool)
  case double(Double)
  case null
  case object(PropertyBag)
  case array([PropertyValue])

  fileprivate func encode(to container: inout KeyedEncodingContainer<DynamicCodingKey>, forKey key: DynamicCodingKey)
    throws
  {
    switch self {
    case .string(let value):
      try container.encode(value, forKey: key)
    case .int(let value):
      try container.encode(value, forKey: key)
    case .bool(let value):
      try container.encode(value, forKey: key)
    case .double(let value):
      try container.encode(value, forKey: key)
    case .null:
      try container.encodeNil(forKey: key)
    case .object(let value):
      try container.encode(value, forKey: key)
    case .array(let value):
      try container.encode(value, forKey: key)
    }
  }

  fileprivate init(from decoder: Decoder, container: KeyedDecodingContainer<DynamicCodingKey>, key: DynamicCodingKey)
    throws
  {
    // Try decoding in order of specificity
    if try container.decodeNil(forKey: key) {
      self = .null
    } else if let value = try? container.decode(Bool.self, forKey: key) {
      // Try bool before int/double as bool can be coerced to numbers
      self = .bool(value)
    } else if let value = try? container.decode(Int.self, forKey: key) {
      self = .int(value)
    } else if let value = try? container.decode(Double.self, forKey: key) {
      self = .double(value)
    } else if let value = try? container.decode(String.self, forKey: key) {
      self = .string(value)
    } else if let value = try? container.decode(PropertyBag.self, forKey: key) {
      self = .object(value)
    } else if let value = try? container.decode([PropertyValue].self, forKey: key) {
      self = .array(value)
    } else {
      throw DecodingError.dataCorruptedError(
        forKey: key,
        in: container,
        debugDescription: "Unsupported property value type"
      )
    }
  }
}

extension PropertyValue: Codable {
  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    switch self {
    case .string(let value):
      try container.encode(value)
    case .int(let value):
      try container.encode(value)
    case .bool(let value):
      try container.encode(value)
    case .double(let value):
      try container.encode(value)
    case .null:
      try container.encodeNil()
    case .object(let value):
      try container.encode(value)
    case .array(let value):
      try container.encode(value)
    }
  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()

    if container.decodeNil() {
      self = .null
    } else if let value = try? container.decode(Bool.self) {
      self = .bool(value)
    } else if let value = try? container.decode(Int.self) {
      self = .int(value)
    } else if let value = try? container.decode(Double.self) {
      self = .double(value)
    } else if let value = try? container.decode(String.self) {
      self = .string(value)
    } else if let value = try? container.decode(PropertyBag.self) {
      self = .object(value)
    } else if let value = try? container.decode([PropertyValue].self) {
      self = .array(value)
    } else {
      throw DecodingError.dataCorruptedError(in: container, debugDescription: "Unsupported property value type")
    }
  }
}

private struct DynamicCodingKey: CodingKey {
  var stringValue: String
  var intValue: Int?

  init(stringValue: String) {
    self.stringValue = stringValue
    self.intValue = nil
  }

  init?(intValue: Int) {
    self.stringValue = "\(intValue)"
    self.intValue = intValue
  }
}
