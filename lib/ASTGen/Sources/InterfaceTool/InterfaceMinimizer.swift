//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftSyntax

extension SyntaxProtocol {
  /// Produce a copy of this syntax node that has been minimized for interface
  /// generation by stripping function bodies that are not required for inlining,
  /// and optionally removing non-public declarations.
  ///
  /// When `removeInternalDecls` is false (the default), all declarations are
  /// kept regardless of access level and only function/accessor bodies are
  /// stripped. When true, the output retains only interface-visible
  /// declarations:
  ///
  /// - `public`, `open`, `package`, and `private` declarations are kept
  /// - `@usableFromInline` declarations are kept
  /// - `@_spi` declarations are kept
  /// - `fileprivate` and plain `internal` declarations are removed
  ///
  /// In both modes:
  /// - Function bodies are stripped unless `@inlinable`, `@_transparent`,
  ///   `@_alwaysEmitIntoClient`, or `@backDeployed`
  /// - `#if` blocks are preserved unchanged (the compiler evaluates them)
  ///
  /// Import handling depends on `removeInternalDecls`:
  /// - When false, all imports are kept unconditionally
  /// - When true, `@_implementationOnly` imports are preserved,
  ///   bare imports follow `internalImportByDefault`, and imports with
  ///   explicit `internal`/`fileprivate` are removed
  public func minimizedForInterface(
    internalImportByDefault: Bool = false,
    removeInternalDecls: Bool = false
  ) -> Syntax {
    let rewriter = InterfaceMinimizer(
      internalImportByDefault: internalImportByDefault,
      removeInternalDecls: removeInternalDecls
    )
    return rewriter.rewrite(Syntax(self)).strippingComments()
  }
}

/// The context in which declarations are being evaluated, used to determine
/// the default visibility of members.
private enum DeclContext {
  case topLevel
  case publicType
  case protocolDecl
  case extensionDecl(isPublic: Bool)

  /// Whether this context is inside a public type declaration.
  var isPublicType: Bool {
    if case .publicType = self { return true }
    return false
  }
}

/// A `SyntaxRewriter` that strips non-inlinable function bodies and optionally
/// removes non-public declarations to produce a minimized source suitable for
/// `.swiftinterface` generation or dependency scanning.
class InterfaceMinimizer: SyntaxRewriter {

  /// When true, bare imports (without an explicit access modifier) are treated
  /// as internal and removed during minimization.
  private let internalImportByDefault: Bool

  /// When true, internal/fileprivate declarations are removed. When false
  /// (the default), all declarations are kept and only bodies are stripped.
  private let removeInternalDecls: Bool

  private var contextStack: [DeclContext] = [.topLevel]

  private var currentContext: DeclContext {
    contextStack.last ?? .topLevel
  }

  init(internalImportByDefault: Bool = false, removeInternalDecls: Bool = false) {
    self.internalImportByDefault = internalImportByDefault
    self.removeInternalDecls = removeInternalDecls
    super.init()
  }

  // MARK: - List-level filtering

  override func visit(_ node: CodeBlockItemListSyntax) -> CodeBlockItemListSyntax {
    return filterCodeBlockItems(node)
  }

  override func visit(_ node: MemberBlockItemListSyntax) -> MemberBlockItemListSyntax {
    return filterMemberBlockItems(node)
  }

  private func filterCodeBlockItems(_ items: CodeBlockItemListSyntax) -> CodeBlockItemListSyntax {
    var newItems: [CodeBlockItemSyntax] = []
    var anyChanged = false

    for index in items.indices {
      let item = items[index]

      guard case .decl(let decl) = item.item else {
        if anyChanged {
          newItems.append(item)
        }
        continue
      }

      let disposition = declarationDisposition(decl)
      switch disposition {
      case .keep:
        let rewritten = rewrite(Syntax(item)).as(CodeBlockItemSyntax.self) ?? item
        if case .decl(let rewrittenDecl) = rewritten.item,
          isRemovableEmptyExtension(rewrittenDecl)
        {
          noteChanged(&anyChanged, &newItems, items, index)
        } else if rewritten.id != item.id {
          noteChanged(&anyChanged, &newItems, items, index)
          newItems.append(rewritten)
        } else if anyChanged {
          newItems.append(item)
        }
      case .remove:
        noteChanged(&anyChanged, &newItems, items, index)
      }
    }

    if !anyChanged {
      return items
    }
    return CodeBlockItemListSyntax(newItems)
  }

  private func filterMemberBlockItems(_ items: MemberBlockItemListSyntax) -> MemberBlockItemListSyntax {
    var newItems: [MemberBlockItemSyntax] = []
    var anyChanged = false

    for index in items.indices {
      let item = items[index]
      let decl = item.decl

      let disposition = declarationDisposition(DeclSyntax(decl))
      switch disposition {
      case .keep:
        let rewritten = rewrite(Syntax(item)).as(MemberBlockItemSyntax.self) ?? item
        if isRemovableEmptyExtension(DeclSyntax(rewritten.decl)) {
          noteChanged(&anyChanged, &newItems, items, index)
        } else if rewritten.id != item.id {
          noteChanged(&anyChanged, &newItems, items, index)
          newItems.append(rewritten)
        } else if anyChanged {
          newItems.append(item)
        }
      case .remove:
        noteChanged(&anyChanged, &newItems, items, index)
      }
    }

    if !anyChanged {
      return items
    }
    return MemberBlockItemListSyntax(newItems)
  }

  private func noteChanged<C: SyntaxCollection>(
    _ anyChanged: inout Bool,
    _ newItems: inout [C.Element],
    _ collection: C,
    _ currentIndex: C.Index
  ) {
    if anyChanged { return }
    anyChanged = true
    newItems.append(contentsOf: collection[..<currentIndex])
  }

  // MARK: - Declaration disposition

  private enum Disposition {
    case keep
    case remove
  }

  private func declarationDisposition(_ decl: DeclSyntax) -> Disposition {
    // #if blocks are always kept (V1 conservative approach).
    if decl.is(IfConfigDeclSyntax.self) {
      return .keep
    }

    // Extensions are always kept at the disposition level — their members
    // are filtered individually in visit(ExtensionDeclSyntax).
    if decl.is(ExtensionDeclSyntax.self) {
      return .keep
    }

    // In body-stripping-only mode, keep all declarations regardless of access
    // level. Only function/accessor bodies are stripped (handled elsewhere).
    if !removeInternalDecls {
      return .keep
    }

    // Protocols are always kept regardless of access level because they
    // may be used as conformance targets by kept declarations.
    if decl.is(ProtocolDeclSyntax.self) {
      return .keep
    }

    // In protocol context, all members are part of the interface.
    if case .protocolDecl = currentContext {
      return .keep
    }

    // Deinitializers in public types should be kept.
    if currentContext.isPublicType, decl.is(DeinitializerDeclSyntax.self) {
      return .keep
    }

    // Enum cases are always kept if the parent enum is kept.
    if decl.is(EnumCaseDeclSyntax.self) {
      return .keep
    }

    // @_exported imports are always kept — they re-export the module
    // and are part of the public interface regardless of access level.
    // @_implementationOnly imports are kept (the compiler needs them for
    // type-checking the interface).
    // Bare imports (no explicit access modifier) are kept when
    // internalImportByDefault is false (Swift 5 mode where they are
    // effectively public) and removed when true.
    // Imports with explicit internal/fileprivate fall through to
    // shouldKeepDecl and are removed; private imports are kept.
    if let importDecl = decl.as(ImportDeclSyntax.self) {
      if hasAttribute(in: importDecl.attributes, named: "_exported") {
        return .keep
      }
      if hasAttribute(in: importDecl.attributes, named: "_implementationOnly") {
        return .keep
      }
      if !hasExplicitAccessModifier(importDecl.modifiers) {
        return internalImportByDefault ? .remove : .keep
      }
      // Falls through to shouldKeepDecl for explicitly-modified imports.
    }

    // For declarations with modifiers and attributes, check access level.
    if let withModifiers = decl.asProtocol(WithModifiersSyntax.self),
      let withAttributes = decl.asProtocol(WithAttributesSyntax.self)
    {
      // In a public extension, members without an explicit access modifier
      // inherit the extension's public access level and should be kept.
      if case .extensionDecl(isPublic: true) = currentContext,
        !hasExplicitAccessModifier(withModifiers.modifiers)
      {
        return .keep
      }

      if shouldKeepDecl(modifiers: withModifiers.modifiers, attributes: withAttributes.attributes) {
        return .keep
      }
      return .remove
    }

    return .keep
  }

  /// An extension that has no members left after filtering and introduces no
  /// conformances can be safely removed.
  private func isRemovableEmptyExtension(_ decl: DeclSyntax) -> Bool {
    guard let ext = decl.as(ExtensionDeclSyntax.self) else { return false }
    return ext.inheritanceClause == nil && ext.memberBlock.members.isEmpty
  }

  // MARK: - Type declarations (push/pop context)

  override func visit(_ node: ClassDeclSyntax) -> DeclSyntax {
    return visitDeclGroup(node, makeDeclSyntax: { DeclSyntax($0) })
  }

  override func visit(_ node: StructDeclSyntax) -> DeclSyntax {
    return visitDeclGroup(node, makeDeclSyntax: { DeclSyntax($0) })
  }

  override func visit(_ node: EnumDeclSyntax) -> DeclSyntax {
    return visitDeclGroup(node, makeDeclSyntax: { DeclSyntax($0) })
  }

  override func visit(_ node: ActorDeclSyntax) -> DeclSyntax {
    return visitDeclGroup(node, makeDeclSyntax: { DeclSyntax($0) })
  }

  override func visit(_ node: ProtocolDeclSyntax) -> DeclSyntax {
    contextStack.append(.protocolDecl)
    defer { contextStack.removeLast() }

    let newMembers = filterMemberBlockItems(node.memberBlock.members)
    if newMembers.id != node.memberBlock.members.id {
      return DeclSyntax(node.with(\.memberBlock, node.memberBlock.with(\.members, newMembers)))
    }
    return DeclSyntax(node)
  }

  override func visit(_ node: ExtensionDeclSyntax) -> DeclSyntax {
    let level = accessLevel(of: node.modifiers)
    let isPublic: Bool
    switch level {
    case .public, .open, .package:
      isPublic = true
    default:
      isPublic = false
    }

    contextStack.append(.extensionDecl(isPublic: isPublic))
    defer { contextStack.removeLast() }

    let newMembers = filterMemberBlockItems(node.memberBlock.members)
    if newMembers.id != node.memberBlock.members.id {
      return DeclSyntax(node.with(\.memberBlock, node.memberBlock.with(\.members, newMembers)))
    }
    return DeclSyntax(node)
  }

  private func visitDeclGroup<D: DeclGroupSyntax & DeclSyntaxProtocol>(
    _ node: D,
    makeDeclSyntax: (D) -> DeclSyntax
  ) -> DeclSyntax {
    contextStack.append(.publicType)
    defer { contextStack.removeLast() }

    let newMembers = filterMemberBlockItems(node.memberBlock.members)
    if newMembers.id != node.memberBlock.members.id {
      return makeDeclSyntax(node.with(\.memberBlock, node.memberBlock.with(\.members, newMembers)))
    }
    return makeDeclSyntax(node)
  }

  // MARK: - Body stripping

  override func visit(_ node: FunctionDeclSyntax) -> DeclSyntax {
    guard node.body != nil else { return DeclSyntax(node) }
    if shouldPreserveBody(attributes: node.attributes) {
      return DeclSyntax(node)
    }
    return DeclSyntax(node.with(\.body, nil))
  }

  override func visit(_ node: InitializerDeclSyntax) -> DeclSyntax {
    guard node.body != nil else { return DeclSyntax(node) }
    if shouldPreserveBody(attributes: node.attributes) {
      return DeclSyntax(node)
    }
    return DeclSyntax(node.with(\.body, nil))
  }

  override func visit(_ node: DeinitializerDeclSyntax) -> DeclSyntax {
    guard node.body != nil else { return DeclSyntax(node) }
    return DeclSyntax(node.with(\.body, nil))
  }

  override func visit(_ node: AccessorDeclSyntax) -> DeclSyntax {
    guard node.body != nil else { return DeclSyntax(node) }

    if let parentDecl = findParentDeclAttributes(from: Syntax(node)),
      shouldPreserveBody(attributes: parentDecl)
    {
      return DeclSyntax(node)
    }
    return DeclSyntax(node.with(\.body, nil))
  }

  override func visit(_ node: VariableDeclSyntax) -> DeclSyntax {
    let preserveBody = shouldPreserveBody(attributes: node.attributes)

    var newBindings: [PatternBindingSyntax] = []
    var anyChanged = false

    for binding in node.bindings {
      var changed = false
      var newBinding = binding

      if let accessorBlock = binding.accessorBlock, !preserveBody {
        let newAccessorBlock = stripAccessorBodies(accessorBlock)
        if newAccessorBlock.id != accessorBlock.id {
          newBinding = newBinding.with(\.accessorBlock, newAccessorBlock)
          changed = true
        }
      }

      if changed {
        anyChanged = true
      }
      newBindings.append(newBinding)
    }

    if anyChanged {
      return DeclSyntax(node.with(\.bindings, PatternBindingListSyntax(newBindings)))
    }
    return DeclSyntax(node)
  }

  override func visit(_ node: SubscriptDeclSyntax) -> DeclSyntax {
    guard let accessorBlock = node.accessorBlock else { return DeclSyntax(node) }
    if shouldPreserveBody(attributes: node.attributes) {
      return DeclSyntax(node)
    }
    let newAccessorBlock = stripAccessorBodies(accessorBlock)
    if newAccessorBlock.id != accessorBlock.id {
      return DeclSyntax(node.with(\.accessorBlock, newAccessorBlock))
    }
    return DeclSyntax(node)
  }

  // MARK: - Accessor body stripping helpers

  private func stripAccessorBodies(_ accessorBlock: AccessorBlockSyntax) -> AccessorBlockSyntax {
    switch accessorBlock.accessors {
    case .accessors(let accessorList):
      var newAccessors: [AccessorDeclSyntax] = []
      var anyChanged = false

      for accessor in accessorList {
        if accessor.body != nil {
          anyChanged = true
          newAccessors.append(accessor.with(\.body, nil))
        } else {
          newAccessors.append(accessor)
        }
      }

      if anyChanged {
        return accessorBlock.with(
          \.accessors,
          .accessors(AccessorDeclListSyntax(newAccessors))
        )
      }
      return accessorBlock

    case .getter:
      let getAccessor = AccessorDeclSyntax(
        accessorSpecifier: .keyword(.get)
      )
      return accessorBlock.with(
        \.accessors,
        .accessors(AccessorDeclListSyntax([getAccessor]))
      )
    }
  }

  private func findParentDeclAttributes(from node: Syntax) -> AttributeListSyntax? {
    var current = node.parent
    while let parent = current {
      if let withAttrs = parent.asProtocol(WithAttributesSyntax.self) {
        return withAttrs.attributes
      }
      current = parent.parent
    }
    return nil
  }
}
