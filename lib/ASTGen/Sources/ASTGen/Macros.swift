//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2022-2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftDiagnostics
import SwiftOperators
import SwiftParser
import SwiftSyntax
import SwiftSyntaxMacros

extension SyntaxProtocol {
  func token(at position: AbsolutePosition) -> TokenSyntax? {
    // If the position isn't within this node at all, return early.
    guard position >= self.position && position < self.endPosition else {
      return nil
    }

    // If we are a token syntax, that's it!
    if let token = Syntax(self).as(TokenSyntax.self) {
      return token
    }

    // Otherwise, it must be one of our children.
    for child in children(viewMode: .sourceAccurate) {
      if let token = child.token(at: position) {
        return token
      }
    }
    fatalError("Children of syntax node do not cover all positions in it")
  }
}

/// Describes a macro that has been "exported" to the C++ part of the
/// compiler, with enough information to interface with the C++ layer.
struct ExportedMacro {
  var macro: Macro.Type
}

struct ExportedExecutableMacro {
  var moduleName: String
  var typeName: String
  var plugin: CompilerPlugin
}

enum MacroPluginKind: UInt8 {
  case InProcess = 0
  case Executable = 1
}

enum MacroRole: UInt8 {
  case Expression = 0x01
  case FreestandingDeclaration = 0x02
  case Accessor = 0x04
  case MemberAttribute = 0x08
  case Member = 0x10
  case Peer = 0x20
  case Conformance = 0x40
}

extension String {
  public init(bufferStart: UnsafePointer<UInt8>?, count: Int) {
    let buffer = UnsafeBufferPointer(start: bufferStart, count: count)
    self.init(decoding: buffer, as: UTF8.self)
  }
}

/// Resolve a reference to type metadata into a macro, if posible.
///
/// Returns an unmanaged pointer to an ExportedMacro instance that describes
/// the specified macro. If there is no macro with the given name, produces
/// nil.
@_cdecl("swift_ASTGen_resolveMacroType")
public func resolveMacroType(
  macroTypePtr: UnsafePointer<UInt8>
) -> UnsafeRawPointer? {
  let macroType = unsafeBitCast(macroTypePtr, to: Any.Type.self)

  guard let macro = macroType as? Macro.Type else {
    return nil
  }

  // Allocate and initialize the exported macro.
  let exportedPtr = UnsafeMutablePointer<ExportedMacro>.allocate(capacity: 1)
  exportedPtr.initialize(to: .init(macro: macro))
  return UnsafeRawPointer(exportedPtr)
}

/// Destroys the given macro.
@_cdecl("swift_ASTGen_destroyMacro")
public func destroyMacro(
  macroPtr: UnsafeMutablePointer<UInt8>
) {
  macroPtr.withMemoryRebound(to: ExportedMacro.self, capacity: 1) { macro in
    macro.deinitialize(count: 1)
    macro.deallocate()
  }
}

@_cdecl("swift_ASTGen_resolveExecutableMacro")
public func resolveExecutableMacro(
  moduleName: UnsafePointer<UInt8>,
  moduleNameLength: Int,
  typeName: UnsafePointer<UInt8>,
  typeNameLength: Int,
  pluginOpaqueHandle: UnsafeMutableRawPointer
) -> UnsafeRawPointer {
  let exportedPtr = UnsafeMutablePointer<ExportedExecutableMacro>.allocate(capacity: 1)
  exportedPtr.initialize(to: .init(
    moduleName: String(bufferStart: moduleName, count: moduleNameLength),
    typeName: String(bufferStart: typeName, count: typeNameLength),
    plugin: CompilerPlugin(opaqueHandle: pluginOpaqueHandle)))
  return UnsafeRawPointer(exportedPtr)
}

@_cdecl("swift_ASTGen_destroyExecutableMacro")
public func destroyExecutableMacro(
  macroPtr: UnsafeMutableRawPointer
) {
  let macroPtr = macroPtr.assumingMemoryBound(to: ExportedExecutableMacro.self)
  macroPtr.deinitialize(count: 1)
  macroPtr.deallocate()
}

/// Allocate a copy of the given string as a UTF-8 string.
func allocateUTF8String(
  _ string: String,
  nullTerminated: Bool = false
) -> (UnsafePointer<UInt8>, Int) {
  var string = string
  return string.withUTF8 { utf8 in
    let capacity = utf8.count + (nullTerminated ? 1 : 0)
    let ptr = UnsafeMutablePointer<UInt8>.allocate(
      capacity: capacity
    )
    if let baseAddress = utf8.baseAddress {
      ptr.initialize(from: baseAddress, count: utf8.count)
    }

    if nullTerminated {
      ptr[utf8.count] = 0
    }

    return (UnsafePointer<UInt8>(ptr), utf8.count)
  }
}

/// Diagnostic message used for thrown errors.
fileprivate struct ThrownErrorDiagnostic: DiagnosticMessage {
  let message: String

  var severity: DiagnosticSeverity { .error }

  var diagnosticID: MessageID {
    .init(domain: "SwiftSyntaxMacros", id: "ThrownErrorDiagnostic")
  }
}


@_cdecl("swift_ASTGen_expandFreestandingMacro")
@usableFromInline
func expandFreestandingMacro(
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
  macroPtr: UnsafeRawPointer,
  macroKind: UInt8,
  discriminatorText: UnsafePointer<UInt8>,
  discriminatorTextLength: Int,
  sourceFilePtr: UnsafeRawPointer,
  sourceLocationPtr: UnsafePointer<UInt8>?,
  expandedSourcePointer: UnsafeMutablePointer<UnsafePointer<UInt8>?>,
  expandedSourceLength: UnsafeMutablePointer<Int>
) -> Int {
  // We didn't expand anything so far.
  expandedSourcePointer.pointee = nil
  expandedSourceLength.pointee = 0

  guard let sourceLocationPtr = sourceLocationPtr else {
    print("NULL source location")
    return -1
  }

  let sourceFilePtr = sourceFilePtr.bindMemory(to: ExportedSourceFile.self, capacity: 1)

  guard let macroSyntax = findSyntaxNodeInSourceFile(
    sourceFilePtr: sourceFilePtr, sourceLocationPtr: sourceLocationPtr, type: Syntax.self) else {
    return 1
  }

  let discriminatorBuffer = UnsafeBufferPointer(
    start: discriminatorText, count: discriminatorTextLength
  )
  let discriminator = String(decoding: discriminatorBuffer, as: UTF8.self)

  let expandedSource: String?
  switch MacroPluginKind(rawValue: macroKind)! {
  case .InProcess:
    expandedSource = expandFreestandingMacroInProcess(
      macroPtr: macroPtr,
      diagEnginePtr: diagEnginePtr,
      macroSyntax: macroSyntax,
      sourceFilePtr: sourceFilePtr,
      discriminator: discriminator)
  case .Executable:
    expandedSource = expandFreestandingMacroIPC(
      macroPtr: macroPtr,
      diagEnginePtr: diagEnginePtr,
      macroSyntax: macroSyntax,
      sourceFilePtr: sourceFilePtr,
      discriminator: discriminator)
  }

  guard var expandedSource = expandedSource else {
    return -1
  }

  // Form the result buffer for our caller.
  expandedSource.withUTF8 { utf8 in
    let evaluatedResultPtr = UnsafeMutablePointer<UInt8>.allocate(capacity: utf8.count + 1)
    if let baseAddress = utf8.baseAddress {
      evaluatedResultPtr.initialize(from: baseAddress, count: utf8.count)
    }
    evaluatedResultPtr[utf8.count] = 0

    expandedSourcePointer.pointee = UnsafePointer(evaluatedResultPtr)
    expandedSourceLength.pointee = utf8.count
  }

  return 0
}

func expandFreestandingMacroIPC(
  macroPtr: UnsafeRawPointer,
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
  macroSyntax: Syntax,
  sourceFilePtr: UnsafePointer<ExportedSourceFile>,
  discriminator: String
) -> String? {

  let macroName: String
  if let exprSyntax = macroSyntax.as(MacroExpansionExprSyntax.self) {
    macroName = exprSyntax.macro.text
  } else if let declSyntax = macroSyntax.as(MacroExpansionDeclSyntax.self) {
    macroName = declSyntax.macro.text
  } else {
    fatalError("unknown syntax")
  }

  let macro = macroPtr.assumingMemoryBound(to: ExportedExecutableMacro.self).pointee

  let message = HostToPluginMessage.expandFreestandingMacro(
    macro: .init(moduleName: macro.moduleName, typeName: macro.typeName, name: macroName),
    discriminator: discriminator,
    syntax: PluginMessage.Syntax(syntax: macroSyntax, in: sourceFilePtr)!)

  do {
    let result = try macro.plugin.sendMessageAndWait(message)
    switch result {
    case .expandFreestandingMacroResult(var expandedSource, let diagnostics):
      if !diagnostics.isEmpty {
        let diagEngine = PluginDiagnosticsEngine(cxxDiagnosticEngine: diagEnginePtr)
        diagEngine.add(exportedSourceFile: sourceFilePtr)

        for diagnostic in diagnostics {
          diagEngine.emit(diagnostic, messageSuffix: " (from macro '\(macroName)')")
        }
      }
      return expandedSource

    default:
      fatalError("unexpected result")
    }
  } catch let error {
    fatalError("\(error)")
  }
}

func expandFreestandingMacroInProcess(
  macroPtr: UnsafeRawPointer,
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
  macroSyntax: Syntax,
  sourceFilePtr: UnsafePointer<ExportedSourceFile>,
  discriminator: String
) -> String? {

  // Create a source manager. This should probably persist and be given to us.
  let sourceManager = SourceManager(cxxDiagnosticEngine: diagEnginePtr)
  sourceManager.insert(sourceFilePtr)

  let context = sourceManager.createMacroExpansionContext(
    discriminator: discriminator
  )

  let macroPtr = macroPtr.bindMemory(to: ExportedMacro.self, capacity: 1)

  let macroName: String
  let evaluatedSyntax: Syntax
  do {
    switch macroPtr.pointee.macro {
    // Handle expression macro.
    case let exprMacro as ExpressionMacro.Type:
      guard let parentExpansion = macroSyntax.asProtocol(
        FreestandingMacroExpansionSyntax.self
      ) else {
        print("not on a macro expansion node: \(macroSyntax.recursiveDescription)")
        return nil
      }

      macroName = parentExpansion.macro.text

      func expandExpressionMacro<Node: FreestandingMacroExpansionSyntax>(
        _ node: Node
      ) throws -> ExprSyntax {
        return try exprMacro.expansion(
          of: sourceManager.detach(
            node,
            foldingWith: OperatorTable.standardOperators
          ),
          in: context
        )
      }

      evaluatedSyntax = Syntax(
        try _openExistential(parentExpansion, do: expandExpressionMacro)
      )

    // Handle declaration macro. The resulting decls are wrapped in a
    // `CodeBlockItemListSyntax`.
    case let declMacro as DeclarationMacro.Type:
      guard let parentExpansion = macroSyntax.as(MacroExpansionDeclSyntax.self) else {
        print("not on a macro expansion node: \(macroSyntax.recursiveDescription)")
        return nil
      }
      macroName = parentExpansion.macro.text
      let decls = try declMacro.expansion(
        of: sourceManager.detach(
          parentExpansion,
          foldingWith: OperatorTable.standardOperators
        ),
        in: context
      )
      evaluatedSyntax = Syntax(CodeBlockItemListSyntax(
        decls.map { CodeBlockItemSyntax(item: .decl($0)) }))

    default:
      print("not an expression macro or a freestanding declaration macro")
      return nil
    }
  } catch {
    // Record the error
    sourceManager.diagnose(
      diagnostic: Diagnostic(
        node: macroSyntax,
        message: ThrownErrorDiagnostic(message: String(describing: error))
      ),
      messageSuffix: " (from macro '\(macroName)')"
    )
    return nil
  }

  // Emit diagnostics accumulated in the context.
  for diag in context.diagnostics {
    sourceManager.diagnose(
      diagnostic: diag,
      messageSuffix: " (from macro '\(macroName)')"
    )
  }

  return evaluatedSyntax.trimmedDescription
}

/// Retrieve a syntax node in the given source file, with the given type.
private func findSyntaxNodeInSourceFile<Node: SyntaxProtocol>(
  sourceFilePtr: UnsafeRawPointer,
  sourceLocationPtr: UnsafePointer<UInt8>?,
  type: Node.Type
) -> Node? {
  guard let sourceLocationPtr = sourceLocationPtr else {
    return nil
  }

  let sourceFilePtr = sourceFilePtr.bindMemory(
    to: ExportedSourceFile.self, capacity: 1
  )

  // Find the offset.
  let buffer = sourceFilePtr.pointee.buffer
  let offset = sourceLocationPtr - buffer.baseAddress!
  if offset < 0 || offset >= buffer.count {
    print("source location isn't inside this buffer")
    return nil
  }

  // Find the token at that offset.
  let sf = sourceFilePtr.pointee.syntax
  guard let token = sf.token(at: AbsolutePosition(utf8Offset: offset)) else {
    print("couldn't find token at offset \(offset)")
    return nil
  }

  // Dig out its parent.
  guard let parentSyntax = token.parent else {
    print("not on a macro expansion node: \(token.recursiveDescription)")
    return nil
  }

  return parentSyntax.as(type)
}

@_cdecl("swift_ASTGen_expandAttachedMacro")
@usableFromInline
func expandAttachedMacro(
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
  macroPtr: UnsafeRawPointer,
  macroKind: UInt8,
  discriminatorText: UnsafePointer<UInt8>,
  discriminatorTextLength: Int,
  rawMacroRole: UInt8,
  customAttrSourceFilePtr: UnsafeRawPointer,
  customAttrSourceLocPointer: UnsafePointer<UInt8>?,
  declarationSourceFilePtr: UnsafeRawPointer,
  attachedTo declarationSourceLocPointer: UnsafePointer<UInt8>?,
  parentDeclSourceFilePtr: UnsafeRawPointer?,
  parentDeclSourceLocPointer: UnsafePointer<UInt8>?,
  expandedSourcePointer: UnsafeMutablePointer<UnsafePointer<UInt8>?>,
  expandedSourceLength: UnsafeMutablePointer<Int>
) -> Int {
  // We didn't expand anything so far.
  expandedSourcePointer.pointee = nil
  expandedSourceLength.pointee = 0

  // Dig out the custom attribute for the attached macro declarations.
  guard let customAttrNode = findSyntaxNodeInSourceFile(
    sourceFilePtr: customAttrSourceFilePtr,
    sourceLocationPtr: customAttrSourceLocPointer,
    type: AttributeSyntax.self
  ) else {
    return 1
  }

  // Dig out the node for the declaration to which the custom attribute is
  // attached.
  guard let declarationNode = findSyntaxNodeInSourceFile(
    sourceFilePtr: declarationSourceFilePtr,
    sourceLocationPtr: declarationSourceLocPointer,
    type: DeclSyntax.self
  ) else {
    return 1
  }

  var parentDeclNode: DeclSyntax?
  if let parentDeclSourceFilePtr = parentDeclSourceFilePtr {
    parentDeclNode = findSyntaxNodeInSourceFile(
      sourceFilePtr: parentDeclSourceFilePtr,
      sourceLocationPtr: parentDeclSourceLocPointer,
      type: DeclSyntax.self
    )
  }

  let customAttrSourceFilePtr = customAttrSourceFilePtr.bindMemory(to: ExportedSourceFile.self, capacity: 1)
  let declarationSourceFilePtr = declarationSourceFilePtr.bindMemory(to: ExportedSourceFile.self, capacity: 1)
  let parentDeclSourceFilePtr = parentDeclSourceFilePtr?.bindMemory(to: ExportedSourceFile.self, capacity: 1)

  let discriminatorBuffer = UnsafeBufferPointer(
    start: discriminatorText, count: discriminatorTextLength
  )
  let discriminator = String(decoding: discriminatorBuffer, as: UTF8.self)

  let expandedSources: [String]?
  switch MacroPluginKind(rawValue: macroKind)! {
  case .Executable:
    expandedSources = expandAttachedMacroIPC(
      diagEnginePtr: diagEnginePtr,
      macroPtr: macroPtr,
      rawMacroRole: rawMacroRole,
      discriminator: discriminator,
      customAttrSourceFilePtr: customAttrSourceFilePtr,
      customAttrNode: customAttrNode,
      declarationSourceFilePtr: declarationSourceFilePtr,
      attachedTo: declarationNode,
      parentDeclSourceFilePtr: parentDeclSourceFilePtr,
      parentDeclNode: parentDeclNode)
  case .InProcess:
    expandedSources = expandAttachedMacroInProcess(
      diagEnginePtr: diagEnginePtr,
      macroPtr: macroPtr,
      rawMacroRole: rawMacroRole,
      discriminator: discriminator,
      customAttrSourceFilePtr: customAttrSourceFilePtr,
      customAttrNode: customAttrNode,
      declarationSourceFilePtr: declarationSourceFilePtr,
      attachedTo: declarationNode,
      parentDeclSourceFilePtr: parentDeclSourceFilePtr,
      parentDeclNode: parentDeclNode)
  }

  guard let expandedSources = expandedSources else {
    return -1
  }

  // Fixup the source.
  var expandedSource: String
  switch MacroRole(rawValue: rawMacroRole)! {
  case .Accessor:
    expandedSource = expandedSources.joined(separator: "\n\n")
  case .Member:
    expandedSource = expandedSources.joined(separator: "\n\n")
  case .MemberAttribute:
    expandedSource = expandedSources.joined(separator: " ")
  case .Peer:
    expandedSource = expandedSources.joined(separator: "\n\n")
  case .Conformance:
    expandedSource = expandedSources.joined(separator: "\n\n")
  case .Expression,
      .FreestandingDeclaration:
    fatalError("unreachable")
  }

  // Form the result buffer for our caller.
  expandedSource.withUTF8 { utf8 in
    let evaluatedResultPtr = UnsafeMutablePointer<UInt8>.allocate(capacity: utf8.count + 1)
    if let baseAddress = utf8.baseAddress {
      evaluatedResultPtr.initialize(from: baseAddress, count: utf8.count)
    }
    evaluatedResultPtr[utf8.count] = 0

    expandedSourcePointer.pointee = UnsafePointer(evaluatedResultPtr)
    expandedSourceLength.pointee = utf8.count
  }

  return 0
}

func expandAttachedMacroIPC(
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
  macroPtr: UnsafeRawPointer,
  rawMacroRole: UInt8,
  discriminator: String,
  customAttrSourceFilePtr: UnsafePointer<ExportedSourceFile>,
  customAttrNode: AttributeSyntax,
  declarationSourceFilePtr: UnsafePointer<ExportedSourceFile>,
  attachedTo declarationNode: DeclSyntax,
  parentDeclSourceFilePtr: UnsafePointer<ExportedSourceFile>?,
  parentDeclNode: DeclSyntax?
) -> [String]? {
  let macroName: String = customAttrNode.attributeName.description
  let macro = macroPtr.assumingMemoryBound(to: ExportedExecutableMacro.self).pointee

  // Map the macro role.
  let macroRole: PluginMessage.MacroRole
  switch MacroRole(rawValue: rawMacroRole)! {
  case .Accessor: macroRole = .accessor
  case .Member: macroRole = .member
  case .MemberAttribute: macroRole = .memberAttribute
  case .Peer: macroRole = .peer
  case .Conformance: macroRole = .conformance
  case
      .Expression,
      .FreestandingDeclaration:
    preconditionFailure("unhandled macro role for attached macro")
  }

  // Prepare syntax nodes to transfer.
  let customAttributeSyntax = PluginMessage.Syntax(
    syntax: Syntax(customAttrNode), in: customAttrSourceFilePtr)!

  let declSyntax = PluginMessage.Syntax(
    syntax: Syntax(declarationNode), in: customAttrSourceFilePtr)!

  let parentDeclSyntax: PluginMessage.Syntax?
  if parentDeclNode != nil {
    parentDeclSyntax = .init(syntax: Syntax(parentDeclNode!), in: parentDeclSourceFilePtr!)!
  } else {
    parentDeclSyntax = nil
  }

  let message = HostToPluginMessage.expandAttachedMacro(
    macro: .init(moduleName: macro.moduleName, typeName: macro.typeName, name: macroName),
    macroRole: macroRole,
    discriminator: discriminator,
    customAttributeSyntax: customAttributeSyntax,
    declSyntax: declSyntax,
    parentDeclSyntax: parentDeclSyntax)
  do {
    let result = try macro.plugin.sendMessageAndWait(message)
    switch result {
    case .expandAttachedMacroResult(let expandedSources, let diagnostics):
      // Form the result buffer for our caller.

      if !diagnostics.isEmpty {
        let diagEngine = PluginDiagnosticsEngine(cxxDiagnosticEngine: diagEnginePtr)
        diagEngine.add(exportedSourceFile: customAttrSourceFilePtr)
        diagEngine.add(exportedSourceFile: declarationSourceFilePtr)
        if let parentDeclSourceFilePtr = parentDeclSourceFilePtr {
          diagEngine.add(exportedSourceFile: parentDeclSourceFilePtr)
        }

        for diagnostic in diagnostics {
          diagEngine.emit(diagnostic, messageSuffix: " (from macro '\(macroName)')")
        }

      }
      return expandedSources

    default:
      fatalError("unexpected result")
    }
  } catch let error {
    fatalError("\(error)")
  }
}

func expandAttachedMacroInProcess(
  diagEnginePtr: UnsafeMutablePointer<UInt8>,
  macroPtr: UnsafeRawPointer,
  rawMacroRole: UInt8,
  discriminator: String,
  customAttrSourceFilePtr: UnsafePointer<ExportedSourceFile>,
  customAttrNode: AttributeSyntax,
  declarationSourceFilePtr: UnsafePointer<ExportedSourceFile>,
  attachedTo declarationNode: DeclSyntax,
  parentDeclSourceFilePtr: UnsafePointer<ExportedSourceFile>?,
  parentDeclNode: DeclSyntax?
) -> [String]? {
  // Get the macro.
  let macroPtr = macroPtr.bindMemory(to: ExportedMacro.self, capacity: 1)
  let macro = macroPtr.pointee.macro
  let macroRole = MacroRole(rawValue: rawMacroRole)

  // Create a source manager covering the files we know about.
  let sourceManager = SourceManager(cxxDiagnosticEngine: diagEnginePtr)
  sourceManager.insert(customAttrSourceFilePtr)
  sourceManager.insert(declarationSourceFilePtr)
  if let parentDeclSourceFilePtr = parentDeclSourceFilePtr {
    sourceManager.insert(parentDeclSourceFilePtr)
  }

  // Create an expansion context
  let context = sourceManager.createMacroExpansionContext(
    discriminator: discriminator
  )

  let macroName = customAttrNode.attributeName.trimmedDescription
  var expandedSources: [String]
  do {
    switch (macro, macroRole) {
    case (let attachedMacro as AccessorMacro.Type, .Accessor):
      let accessors = try attachedMacro.expansion(
        of: sourceManager.detach(
          customAttrNode,
          foldingWith: OperatorTable.standardOperators
        ),
        providingAccessorsOf: sourceManager.detach(declarationNode),
        in: context
      )

      // Form a buffer of accessor declarations to return to the caller.
      expandedSources = accessors.map {
        $0.trimmedDescription
      }

    case (let attachedMacro as MemberAttributeMacro.Type, .MemberAttribute):
      // Dig out the node for the parent declaration of the to-expand
      // declaration. Only member attribute macros need this.
      guard let parentDeclNode = parentDeclNode,
            let parentDeclGroup = parentDeclNode.asProtocol(DeclGroupSyntax.self)
      else {
        return nil
      }

      // Local function to expand a member atribute macro once we've opened up
      // the existential.
      func expandMemberAttributeMacro<Node: DeclGroupSyntax>(
        _ node: Node
      ) throws -> [AttributeSyntax] {
        return try attachedMacro.expansion(
          of: sourceManager.detach(
            customAttrNode,
            foldingWith: OperatorTable.standardOperators
          ),
          attachedTo: sourceManager.detach(node),
          providingAttributesFor: sourceManager.detach(declarationNode),
          in: context
        )
      }

      let attributes = try _openExistential(
        parentDeclGroup, do: expandMemberAttributeMacro
      )

      // Form a buffer containing an attribute list to return to the caller.
      expandedSources = attributes.map {
        $0.trimmedDescription
      }

    case (let attachedMacro as MemberMacro.Type, .Member):
      guard let declGroup = declarationNode.asProtocol(DeclGroupSyntax.self)
      else {
        return nil
      }

      // Local function to expand a member macro once we've opened up
      // the existential.
      func expandMemberMacro<Node: DeclGroupSyntax>(
        _ node: Node
      ) throws -> [DeclSyntax] {
        return try attachedMacro.expansion(
          of: sourceManager.detach(
            customAttrNode,
            foldingWith: OperatorTable.standardOperators
          ),
          providingMembersOf: sourceManager.detach(node),
          in: context
        )
      }

      let members = try _openExistential(declGroup, do: expandMemberMacro)

      // Form a buffer of member declarations to return to the caller.
      expandedSources = members.map {
        $0.trimmedDescription
      }

    case (let attachedMacro as PeerMacro.Type, .Peer):
      let peers = try attachedMacro.expansion(
        of: sourceManager.detach(
          customAttrNode,
          foldingWith: OperatorTable.standardOperators
        ),
        providingPeersOf: sourceManager.detach(
          declarationNode,
          foldingWith: OperatorTable.standardOperators
        ),
        in: context
      )

      // Form a buffer of peer declarations to return to the caller.
      expandedSources = peers.map {
        $0.trimmedDescription
      }

    case (let attachedMacro as ConformanceMacro.Type, .Conformance):
      guard let declGroup = declarationNode.asProtocol(DeclGroupSyntax.self),
            let identified = declarationNode.asProtocol(IdentifiedDeclSyntax.self) else {
        return nil
      }

      // Local function to expand a conformance macro once we've opened up
      // the existential.
      func expandConformanceMacro<Node: DeclGroupSyntax>(
        _ node: Node
      ) throws -> [(TypeSyntax, GenericWhereClauseSyntax?)] {
        return try attachedMacro.expansion(
          of: sourceManager.detach(
            customAttrNode,
            foldingWith: OperatorTable.standardOperators
          ),
          providingConformancesOf: sourceManager.detach(node),
          in: context
        )
      }

      let conformances = try _openExistential(
        declGroup, do: expandConformanceMacro
      )

      // Form a buffer of extension declarations to return to the caller.
      expandedSources = conformances.map { typeSyntax, whereClause in
        let typeName = identified.identifier.trimmedDescription
        let protocolName = typeSyntax.trimmedDescription
        let whereClause = whereClause?.trimmedDescription ?? ""
        return "extension \(typeName) : \(protocolName) \(whereClause) {}"
      }

    default:
      print("\(macroPtr) does not conform to any known attached macro protocol")
      return nil
    }
  } catch {
    // Record the error
    // FIXME: Need to decide where to diagnose the error:
    sourceManager.diagnose(
      diagnostic: Diagnostic(
        node: Syntax(declarationNode),
        message: ThrownErrorDiagnostic(message: String(describing: error))
      ),
      messageSuffix: " (from macro '\(macroName)')"
    )

    return nil
  }

  // Emit diagnostics accumulated in the context.
  for diag in context.diagnostics {
    sourceManager.diagnose(
      diagnostic: diag,
      messageSuffix: " (from macro '\(macroName)')"
    )
  }

  return expandedSources
}
