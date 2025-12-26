//===----------------------------------------------------------------------===//
//
// SwiftDiagnosticsToSARIF.swift
// Converter from Swift Diagnostics to SARIF format
//
//===----------------------------------------------------------------------===//

import Foundation

#if compiler(>=6)
public import SwiftSyntax
public import SwiftDiagnostics
#else
import SwiftSyntax
import SwiftDiagnostics
#endif

/// Information about the tool that produced the diagnostics.
public struct ToolInfo {
  public let name: String
  public let version: String
  public let informationUri: String
  public let organization: String

  public init(
    name: String,
    version: String,
    informationUri: String,
    organization: String
  ) {
    self.name = name
    self.version = version
    self.informationUri = informationUri
    self.organization = organization
  }
}

public struct DiagnosticInfo {
  public let filePath: String
  let tree: SourceFileSyntax
  var diagnostics: [Diagnostic]
  let converter: SourceLocationConverter

  public init(
    filePath: String,
    tree: SourceFileSyntax,
    diagnostics: [Diagnostic]
  ) {
    self.filePath = filePath
    self.tree = tree
    self.diagnostics = diagnostics
    self.converter = SourceLocationConverter(fileName: filePath, tree: tree)
  }

  public mutating func add(diagnostic: Diagnostic) {
    diagnostics.append(diagnostic)
  }

  public func getSourceLocation(position: AbsolutePosition) -> SourceLocation {
    return converter.location(for: position)
  }

  public func convertToSARIFResults(artifactIndex: Int) -> [Result] {
    diagnostics.map { diagnostic -> Result in
      convertToSARIFResult(artifactIndex: artifactIndex, diagnostic: diagnostic)
    }
  }

  private func convertToSARIFResult(
    artifactIndex: Int,
    diagnostic: Diagnostic
  ) -> Result {
    let ruleId = convertToSARIFRuleId(messageID: diagnostic.diagnosticID)
    let message = convertToSARIFMessage(message: diagnostic.message)
    let level = convertToSARIFSeverity(severity: diagnostic.diagMessage.severity)
    let location = convertToSARIFLocation(
      artifactIndex: artifactIndex,
      position: diagnostic.position,
      messageText: nil
    )

    let relatedLocations = diagnostic.notes.map { note -> Location in
      convertToSARIFLocation(
        artifactIndex: artifactIndex,
        position: note.position,
        messageText: note.message
      )
    }

    let fixes = diagnostic.fixIts.compactMap { fixIt -> Fix? in
      return convertToSARIFFix(artifactIndex: artifactIndex, fixIt: fixIt)
    }

    return Result(
      ruleId: ruleId,
      message: message,
      level: level,
      kind: .fail,
      locations: [location],
      fixes: fixes.isEmpty ? nil : fixes,
      relatedLocations: relatedLocations.isEmpty ? nil : relatedLocations
    )
  }

  private func convertToSARIFRuleId(messageID: MessageID) -> String {
    // MessageID contains domain and id, but they're private.
    // For now, we create a string representation for the rule ID
    return String(describing: messageID)
  }

  private func convertToSARIFMessage(message: String) -> Message {
    return Message(text: message)
  }

  private func convertToSARIFSeverity(severity: DiagnosticSeverity) -> Result.Level {
    switch severity {
    case .error:
      return .error
    case .warning:
      return .warning
    case .note:
      return .note
    case .remark:
      return .note
    }
  }

  private func convertToSARIFLocation(
    artifactIndex: Int,
    position: AbsolutePosition,
    messageText: String?
  ) -> Location {

    let sourceLocation = getSourceLocation(position: position)

    let region = Region(
      startLine: sourceLocation.line,
      startColumn: sourceLocation.column
    )

    let artifactLocation = ArtifactLocation(index: artifactIndex)

    let physicalLocation = PhysicalLocation(
      artifactLocation: artifactLocation,
      region: region
    )

    let message = (messageText != nil) ? Message(text: messageText!) : nil

    // Compute logical locations (function, class, etc.) containing this position
    let logicalLocations = convertToSARIFLogicalLocations(position: position)

    return Location(
      physicalLocation: physicalLocation,
      logicalLocations: logicalLocations,
      message: message
    )
  }

  private func convertToSARIFFix(artifactIndex: Int, fixIt: FixIt) -> Fix? {
    let edits = fixIt.edits
    guard !edits.isEmpty else { return nil }

    let replacements = edits.map { edit -> Replacement in convertToSARIFReplacement(edit: edit) }

    let artifactLocation = ArtifactLocation(index: artifactIndex)
    let artifactChange = ArtifactChange(artifactLocation: artifactLocation, replacements: replacements)

    let description = Message(text: fixIt.message.message)

    return Fix(description: description, artifactChanges: [artifactChange])
  }

  private func convertToSARIFReplacement(edit: SourceEdit) -> Replacement {
    let startLocation = getSourceLocation(position: edit.range.lowerBound)
    let endLocation = getSourceLocation(position: edit.range.upperBound)

    let deletedRegion = Region(
      startLine: startLocation.line,
      startColumn: startLocation.column,
      endLine: endLocation.line,
      endColumn: endLocation.column
    )

    let insertedContent = edit.replacement.isEmpty ? nil : ArtifactContent(text: edit.replacement)

    return Replacement(deletedRegion: deletedRegion, insertedContent: insertedContent)
  }

  private func convertToSARIFLogicalLocations(position: AbsolutePosition) -> [LogicalLocation]? {

    // Find the token at this position
    guard let token = tree.token(at: position) else {
      return nil
    }

    var currentNode: Syntax? = Syntax(token)
    var nodes: [Syntax] = []

    // Walk up the syntax tree to find enclosing declarations
    while let node = currentNode {
      nodes.append(node)
      currentNode = node.parent
    }

    nodes.reverse()

    var parentIndex: Int? = nil
    var fullyQualifiedParentName = ""

    let logicalLocations: [LogicalLocation] = nodes.flatMap {
      node -> LogicalLocation? in
      let maybeLocation = convertToSARIFLogicalLocation(
        node: node,
        parentIndex: parentIndex,
        fullyQualifiedParentName: fullyQualifiedParentName
      )
      if let location = maybeLocation {
        parentIndex = parentIndex == nil ? 0 : (parentIndex! + 1)
        fullyQualifiedParentName = location.fullyQualifiedName
      }
      return maybeLocation
    }

    return logicalLocations.isEmpty ? nil : logicalLocations
  }

  private func convertToSARIFLogicalLocation(node: Syntax, parentIndex: Int?, fullyQualifiedParentName: String)
    -> LogicalLocation?
  {
    if let (name, kind) = getSyntaxInfo(node: node) {
      let fullyQualifiedName = fullyQualifiedParentName == "" ? name : (fullyQualifiedParentName + "." + name)
      return LogicalLocation(
        name: name,
        fullyQualifiedName: fullyQualifiedName,
        kind: kind,
        parentIndex: parentIndex
      )
    } else {
      return nil
    }
  }

  private func getSyntaxInfo(node: Syntax) -> (String, String)? {
    if let funcDecl = node.as(FunctionDeclSyntax.self) {
      return (funcDecl.name.text, "function")
    } else if let initDecl = node.as(InitializerDeclSyntax.self) {
      return ("init", "function")
    } else if let deinitDecl = node.as(DeinitializerDeclSyntax.self) {
      return ("deinit", "function")
    } else if let subscriptDecl = node.as(SubscriptDeclSyntax.self) {
      return ("subscript", "function")
    } else if let accessorDecl = node.as(AccessorDeclSyntax.self) {
      return (accessorDecl.accessorSpecifier.text, "function")
    } else if let classDecl = node.as(ClassDeclSyntax.self) {
      return (classDecl.name.text, "class")
    } else if let structDecl = node.as(StructDeclSyntax.self) {
      return (structDecl.name.text, "struct")
    } else if let enumDecl = node.as(EnumDeclSyntax.self) {
      return (enumDecl.name.text, "enum")
    } else if let actorDecl = node.as(ActorDeclSyntax.self) {
      return (actorDecl.name.text, "class")
    } else if let protocolDecl = node.as(ProtocolDeclSyntax.self) {
      return (protocolDecl.name.text, "protocol")
    } else if let extensionDecl = node.as(ExtensionDeclSyntax.self) {
      return (extensionDecl.extendedType.trimmedDescription, "extension")
    } else if let typeAliasDecl = node.as(TypeAliasDeclSyntax.self) {
      return (typeAliasDecl.name.text, "type")
    } else if let associatedTypeDecl = node.as(AssociatedTypeDeclSyntax.self) {
      return (associatedTypeDecl.name.text, "type")
    } else if let macroDecl = node.as(MacroDeclSyntax.self) {
      return (macroDecl.name.text, "macro")
    } else if let varDecl = node.as(VariableDeclSyntax.self) {
      // For variable declarations, try to get the first binding's name
      if let binding = varDecl.bindings.first,
        let pattern = binding.pattern.as(IdentifierPatternSyntax.self)
      {
        return (pattern.identifier.text, "variable")
      } else {
        return ("variable", "variable")
      }
    } else if let enumCaseDecl = node.as(EnumCaseDeclSyntax.self) {
      // For enum cases, try to get the first element's name
      if let element = enumCaseDecl.elements.first {
        return (element.name.text, "enumMember")
      } else {
        return ("case", "enumMember")
      }
    } else if let importDecl = node.as(ImportDeclSyntax.self) {
      return (importDecl.path.description.trimmingCharacters(in: .whitespaces), "module")
    } else if let operatorDecl = node.as(OperatorDeclSyntax.self) {
      return (operatorDecl.name.text, "function")
    } else if let precedenceGroupDecl = node.as(PrecedenceGroupDeclSyntax.self) {
      return (precedenceGroupDecl.name.text, "type")
    } else {
      return nil
    }
  }
}

/// Converts diagnostics from multiple files to a single SARIF log.
/// - Parameter diagnosticsByFile: An array of tuples containing file URLs, diagnostics, and syntax trees.
/// - Returns: A SARIF log containing all converted diagnostics.
public func convertToSARIFLog(
  toolInfo: ToolInfo,
  diagnosticInfoArray: [DiagnosticInfo],
  invocation: Invocation
) -> SARIFLog {

  let tool = convertToSARIFTool(toolInfo: toolInfo)
  let (artifactArray, artifactMap) = buildSARIFArtifactCache(diagnosticInfoArray: diagnosticInfoArray)
  let results = diagnosticInfoArray.flatMap { diagnosticInfo -> [Result] in
    let artifactIndex = artifactMap[diagnosticInfo.filePath]!
    return diagnosticInfo.convertToSARIFResults(artifactIndex: artifactIndex)
  }

  let run = Run(
    tool: tool,
    invocations: [invocation],
    results: results,
    artifacts: artifactArray
  )

  return SARIFLog(runs: [run])
}

fileprivate func convertToSARIFTool(toolInfo: ToolInfo) -> Tool {
  return Tool(
    driver: ToolComponent(
      name: toolInfo.name,
      version: toolInfo.version,
      informationUri: toolInfo.informationUri,
      organization: toolInfo.organization
    )
  )
}

fileprivate func buildSARIFArtifactCache(diagnosticInfoArray: [DiagnosticInfo]) -> ([Artifact], [String: Int]) {
  var artifactArray: [Artifact] = []
  var artifactMap: [String: Int] = [:]

  for diagnosticInfo in diagnosticInfoArray {
    let filePath = diagnosticInfo.filePath

    // don't add redundant artifact
    if artifactMap[filePath] != nil {
      continue
    }

    let index = artifactArray.count
    artifactMap[filePath] = index

    let artifact = Artifact(
      location: ArtifactLocation(uri: filePath),
      mimeType: "text/x-swift",
      sourceLanguage: "swift"
    )

    artifactArray.append(artifact)
  }

  return (artifactArray, artifactMap)
}
