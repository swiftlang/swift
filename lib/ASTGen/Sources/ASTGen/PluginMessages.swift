//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

internal enum HostToPluginMessage: Codable {
  case getCapability

  case expandFreestandingMacro(
    macro: PluginMessage.MacroReference,
    discriminator: String,
    syntax: PluginMessage.Syntax)

  case expandAttachedMacro(
    macro: PluginMessage.MacroReference,
    macroRole: PluginMessage.MacroRole,
    discriminator: String,
    customAttributeSyntax: PluginMessage.Syntax,
    declSyntax: PluginMessage.Syntax,
    parentDeclSyntax: PluginMessage.Syntax?)
}

internal enum PluginToHostMessage: Codable {
  case expandFreestandingMacroResult(
    expandedSource: String?,
    diagnostics: [PluginMessage.Diagnostic])

  case expandAttachedMacroResult(
    expandedSources: [String]?,
    diagnostics: [PluginMessage.Diagnostic])

  case getCapabilityResult(capability: PluginMessage.PluginCapability)
}

/*namespace*/ internal enum PluginMessage {
  static var PROTOCOL_VERSION_NUMBER: Int { 2 } // Added 'MacroRole.conformance'

  struct PluginCapability: Codable {
    var protocolVersion: Int
  }

  static var capability: PluginCapability {
    PluginCapability(protocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
  }

  struct MacroReference: Codable {
    var moduleName: String
    var typeName: String

    // The name of 'macro' declaration the client is using.
    var name: String
  }

  enum MacroRole: String, Codable {
    case expression
    case freeStandingDeclaration
    case accessor
    case memberAttribute
    case member
    case peer
    case conformance
  }

  struct SourceLocation: Codable {
    var fileID: String
    var fileName: String
    var offset: Int
    var line: Int
    var column: Int
  }

  struct Diagnostic: Codable {
    enum Severity: String, Codable {
      case error
      case warning
      case note
    }
    struct Position: Codable {
      var fileName: String
      var offset: Int
    }
    struct PositionRange: Codable {
      var fileName: String
      var startOffset: Int
      var endOffset: Int
    }
    struct Note: Codable {
      var position: Position
      var message: String
    }
    struct FixIt: Codable {
      struct Change: Codable {
        var range: PositionRange
        var newText: String
      }
      var message: String
      var changes: [Change]
    }
    var message: String
    var severity: Severity
    var position: Position
    var highlights: [PositionRange]
    var notes: [Note]
    var fixIts: [FixIt]
  }

  struct Syntax: Codable {
    enum Kind: String, Codable {
      case declaration
      case statement
      case expression
      case type
      case pattern
      case attribute
    }
    var kind: Kind
    var source: String
    var location: SourceLocation
  }
}
