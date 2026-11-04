//===--- DeclAttrs.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import SwiftDiagnostics
import SwiftIfConfig

@_spi(ExperimentalLanguageFeatures) @_spi(RawSyntax) @_spi(Compiler) import SwiftSyntax

extension ASTGenVisitor {
  struct DeclAttributesResult {
    var attributes: BridgedDeclAttributes
    var staticSpelling: BridgedStaticSpelling
    var staticLoc: SourceLoc
  }

  func generateDeclAttributes(_ node: some WithAttributesSyntax & WithModifiersSyntax, allowStatic: Bool) -> DeclAttributesResult {
    var attrs = BridgedDeclAttributes()
    var staticSpelling: BridgedStaticSpelling = .none
    var staticLoc: SourceLoc = nil

    // Comments.
    COMMENT: if
      self.ctx.langOpts.attachCommentsToDecls,
      let firstTok = node.firstToken(viewMode: .sourceAccurate)
    {
      var pos = firstTok.position
      for p in firstTok.leadingTrivia {
        switch p {
        // 'RawDocCommentAttr' takes the range '[start of any comments, start of the token text)'.
        case .docLineComment, .docBlockComment, .lineComment, .blockComment:
          let commentLength = firstTok.positionAfterSkippingLeadingTrivia.utf8Offset - pos.utf8Offset
          let range = self.generateCharSourceRange(start: pos, length: SourceLength(utf8Length: commentLength))
          let attr = BridgedRawDocCommentAttr.createParsed(self.ctx, range: range)
          attrs.add(attr.asDeclAttribute)
          break COMMENT
        default:
          break
        }
        pos = pos.advanced(by: p.sourceLength.utf8Length)
      }
    }

    func addAttribute(_ attr: BridgedDeclAttribute?) {
      guard let attr else {
        return
      }
      // Diagnose, and drop, an attribute that may appear only once but
      // already did.
      let duplicate = attrs.checkDuplicate(attr)
      if duplicate.isDuplicate {
        if duplicate.hasDifferentAccessLevel {
          self.diagnose(.multiple_access_level_modifiers, at: duplicate.newLoc)
          self.diagnose(.previous_access_level_modifier, at: duplicate.previousLoc)
        } else {
          self.diagnose(.duplicate_attribute(duplicate.isModifier), at: duplicate.newLoc)
          self.diagnose(.previous_attribute(duplicate.isModifier), at: duplicate.previousLoc)
        }
        return
      }
      attrs.add(attr)
    }

    // '@' attributes.
    self.generateDeclAttributes(attributeList: node.attributes, handler: addAttribute(_:))

    var sawStatic = false
    func genStatic(node: DeclModifierSyntax, spelling: BridgedStaticSpelling) {
      // Only the first 'static'/'class' modifier counts.
      if sawStatic {
        self.diagnose(
          .decl_already_static(spelling),
          at: node,
          fixIts: [self.tidyRemovalFixIt(node)]
        )
        return
      }
      sawStatic = true

      // TODO: Diagnose duplicated attrs.
      // TODO: Diagnose if allowStatic is false.
      // TODO: Diagnose 'class' for non-class decls.

      // 'static'/'class' requires a type context. The C++ parser reports this and
      // then drops the static-ness entirely; keeping it would leave a static decl
      // at file scope, which downstream code is not prepared for -- a static
      // property there reaches `getSelfInterfaceType()` and asserts.
      if !self.declContext.isTypeContext {
        let diagnostic: CompilerDiagnostic =
          node.parent?.parent?.is(FunctionDeclSyntax.self) == true
          ? .static_func_decl_global_scope(spelling)
          : .static_var_decl_global_scope(spelling)
        // The C++ parser reports this once the modifier list has been consumed,
        // so it points at the declaration's introducer keyword rather than at the
        // modifier -- the 'let' in 'static let x', say.
        let introducer = node.parent?.as(DeclModifierListSyntax.self)?.last?
          .nextToken(viewMode: .sourceAccurate)
        self.diagnose(
          diagnostic,
          at: introducer.map(Syntax.init) ?? Syntax(node),
          fixIts: [self.tidyRemovalFixIt(node)]
        )
        return
      }

      // `class` members only exist in classes: in a struct, enum or protocol the
      // C++ parser reports it and treats it as `static`.
      var spelling = spelling
      if spelling == .class {
        let declNode = node.parent?.parent
        var container = declNode?.parent
        while let current = container, current.is(MemberBlockItemSyntax.self) || current.is(MemberBlockItemListSyntax.self)
          || current.is(MemberBlockSyntax.self)
        {
          container = current.parent
        }
        let isProtocol = container?.is(ProtocolDeclSyntax.self) == true
        if container?.is(StructDeclSyntax.self) == true || container?.is(EnumDeclSyntax.self) == true || isProtocol {
          let diagnostic: CompilerDiagnostic?
          if declNode?.is(FunctionDeclSyntax.self) == true {
            diagnostic = .class_func_not_in_class(isProtocol)
          } else if declNode?.is(VariableDeclSyntax.self) == true {
            diagnostic = .class_var_not_in_class(isProtocol)
          } else if declNode?.is(SubscriptDeclSyntax.self) == true {
            diagnostic = .class_subscript_not_in_class(isProtocol)
          } else {
            diagnostic = nil
          }
          if let diagnostic {
            // Reported at the declaration's introducer, after the modifiers.
            let introducer = node.parent?.as(DeclModifierListSyntax.self)?.last?
              .nextToken(viewMode: .sourceAccurate)
            self.diagnose(
              diagnostic,
              at: introducer.map(Syntax.init) ?? Syntax(node),
              fixIts: [.init(replace: self.generateCharSourceRange(node), with: "static")]
            )
            spelling = .static
          }
        }
      }

      // FIXME: Model static/class as a DeclAttribute.
      staticLoc = self.generateSourceLoc(node)
      staticSpelling = spelling
    }

    // The syntax node the modifiers belong to.
    let declSyntax = Syntax(node)
    // The first fixity modifier of a function, which the later ones contradict.
    var firstFixity: BridgedDeclAttribute?

    for node in node.modifiers {
      // A function takes at most one of 'prefix' and 'postfix', and not 'infix'.
      if declSyntax.is(FunctionDeclSyntax.self) {
        let kind: swift.DeclAttrKind? =
          switch node.name.text {
          case "prefix": .Prefix
          case "postfix": .Postfix
          case "infix": .Infix
          default: nil
          }
        if let kind {
          let attr = BridgedDeclAttribute.createSimple(self.ctx, kind: kind, atLoc: nil, nameLoc: self.generateSourceLoc(node))
          if let firstFixity {
            self.diagnose(
              .mutually_exclusive_decl_attrs(attr, firstFixity),
              at: node,
              fixIts: [self.tidyRemovalFixIt(node)]
            )
            continue
          }
          if kind == .Infix {
            self.diagnose(.invalid_infix_on_func, at: node, fixIts: [self.tidyRemovalFixIt(node)])
            continue
          }
          firstFixity = attr
        }
      }

      // `async func f()`: the modifier belongs after the parameter list. The
      // function is still async; the modifier is not recorded as an attribute.
      if node.name.text == "async", node.detail == nil,
        let rightParen = declSyntax.as(FunctionDeclSyntax.self)?.signature.parameterClause.rightParen
          ?? declSyntax.as(InitializerDeclSyntax.self)?.signature.parameterClause.rightParen
      {
        self.diagnose(
          .async_func_modifier,
          at: node,
          fixIts: [
            self.tidyRemovalFixIt(node),
            .init(
              replace: self.generateCharSourceRange(
                start: rightParen.endPositionBeforeTrailingTrivia,
                length: SourceLength(utf8Length: 0)
              ),
              with: " async"
            ),
          ]
        )
        continue
      }

      switch node.name.keywordKind {
      case .static:
        genStatic(node: node, spelling: .static)
      case .class:
        genStatic(node: node, spelling: .class)
      default:
        addAttribute(self.generate(declModifier: node))
      }
    }

    return DeclAttributesResult(
      attributes: attrs,
      staticSpelling: staticSpelling,
      staticLoc: staticLoc
    )
  }

  func generateDeclAttributes(attributeList node: AttributeListSyntax, handler: (BridgedDeclAttribute) -> Void) {
    let outerExposureKinds = self.seenExposureKinds
    self.seenExposureKinds = []
    let outerExternKinds = self.seenExternKinds
    self.seenExternKinds = []
    defer {
      self.seenExposureKinds = outerExposureKinds
      self.seenExternKinds = outerExternKinds
    }

    visitIfConfigElements(node, of: AttributeSyntax.self) { element in
      switch element {
      case .ifConfigDecl(let ifConfigDecl):
        return .ifConfigDecl(ifConfigDecl)
      case .attribute(let attribute):
        return .underlying(attribute)
      }
    } body: { attribute in
      self.generateDeclAttribute(attribute: attribute, handler: handler)
    }
  }
}

// MARK: - Decl attributes
extension ASTGenVisitor {
  func generateDeclAttribute(attribute node: AttributeSyntax, handler: (BridgedDeclAttribute) -> Void) {
    func handle(_ attr: BridgedDeclAttribute?) {
      if let attr {
        handler(attr)
      }
    }

    if let identTy = node.attributeName.as(IdentifierTypeSyntax.self) {
      let attrName = identTy.name.rawText
      let attrKind: swift.DeclAttrKind?
      do {
        let bridgedOptional = BridgedOptionalDeclAttrKind(from: attrName.bridged)
        attrKind = if identTy.moduleSelector == nil && bridgedOptional.hasValue {
          bridgedOptional.value
        } else {
          nil
        }
      }

      // A declaration modifier spelled as an attribute (`@override`) is
      // accepted, with an error and a fix-it that removes the '@'.
      if let attrKind, attrKind != .Rethrows, attrKind != .Reasync,
        BridgedDeclAttribute.isDeclModifier(attrKind)
      {
        self.diagnose(
          .cskeyword_not_attribute(String(syntaxText: attrName)),
          at: node.atSign,
          fixIts: [.init(remove: self.generateCharSourceRange(node.atSign))]
        )
      }

      // The attribute parsers do not accept a trailing comma, so it ends the
      // attribute early. The swift-syntax parser accepts one in any argument
      // list; the cases it diagnoses itself have an error in the tree.
      if attrKind != nil, !self.ctx.langOpts.hasFeature(.TrailingComma), !node.hasError {
        if let trailingComma = node.arguments?.as(LabeledExprListSyntax.self)?.last?.trailingComma {
          self.diagnose(.attr_expected_rparen(String(syntaxText: attrName), false), at: trailingComma)
          return
        }
        if let trailingComma = node.arguments?.as(AvailabilityArgumentListSyntax.self)?.last?.trailingComma {
          self.diagnose(
            .avail_query_expected_platform_name,
            at: trailingComma.nextToken(viewMode: .sourceAccurate).map(Syntax.init) ?? Syntax(trailingComma)
          )
          return
        }
      }

      switch attrKind {
      case .ABI:
        return handle(self.generateABIAttr(attribute: node)?.asDeclAttribute)
      case .Alignment:
        return handle(self.generateAlignmentAttr(attribute: node)?.asDeclAttribute)
      case .AllowFeatureSuppression:
        return handle(self.generateAllowFeatureSuppressionAttr(attribute: node, attrName: attrName)?.asDeclAttribute)
      case .Available:
        return self.generateAvailableAttr(attribute: node, attrName: attrName).forEach { handle($0.asDeclAttribute) }
      case .BackDeployed:
        return self.generateBackDeployedAttr(attribute: node).forEach { handle($0.asDeclAttribute) }
      case .CDecl:
        if attrName.first == UInt8(ascii: "_") { self.diagnoseNonLocalAttribute(node, attrName: attrName) }
        return handle(self.generateCDeclAttr(attribute: node)?.asDeclAttribute)
      case .COM:
        return handle(self.generateCOMAttr(attribute: node)?.asDeclAttribute)
      case .CxxDecl:
        return handle(self.generateCxxDeclAttr(attribute: node)?.asDeclAttribute)
      case .Derivative:
        self.diagnoseNonLocalAttribute(node, attrName: attrName)
        return handle(self.generateDerivativeAttr(attribute: node)?.asDeclAttribute)
      case .Differentiable:
        return handle(self.generateDifferentiableAttr(attribute: node)?.asDeclAttribute)
      case .DynamicReplacement:
        return handle(self.generateDynamicReplacementAttr(attribute: node)?.asDeclAttribute)
      case .Documentation:
        return handle(self.generateDocumentationAttr(attribute: node)?.asDeclAttribute)
      case .Effects:
        return handle(self.generateEffectsAttr(attribute: node)?.asDeclAttribute)
      case .Exclusivity:
        return handle(self.generateExclusivityAttr(attribute: node)?.asDeclAttribute)
      case .Export:
        return handle(self.generateExportAttr(attribute: node)?.asDeclAttribute)
      case .Expose:
        self.diagnoseNonLocalAttribute(node, attrName: attrName)
        return handle(self.generateExposeAttr(attribute: node)?.asDeclAttribute)
      case .Extern:
        return handle(self.generateExternAttr(attribute: node)?.asDeclAttribute)
      case .Implements:
        return handle(self.generateImplementsAttr(attribute: node)?.asDeclAttribute)
      case .Inline:
        return handle(self.generateInlineAttr(attribute: node)?.asDeclAttribute)
      case .Lifetime:
        return handle(self.generateLifetimeAttr(attribute: node)?.asDeclAttribute)
      case .MacroRole:
        return handle(self.generateMacroRoleAttr(attribute: node, attrName: attrName)?.asDeclAttribute)
      case .NonSendable:
        return handle(self.generateNonSendableAttr(attribute: node)?.asDeclAttribute)
      case .Nonexhaustive:
        return handle(self.generateNonexhaustiveAttr(attribute: node)?.asDeclAttribute)
      case .ObjC:
        return handle(self.generateObjCAttr(attribute: node)?.asDeclAttribute)
      case .ObjCImplementation:
        return handle(self.generateObjCImplementationAttr(attribute: node)?.asDeclAttribute)
      case .ObjCRuntimeName:
        return handle(self.generateObjCRuntimeNameAttr(attribute: node)?.asDeclAttribute)
      case .Optimize:
        return handle(self.generateOptimizeAttr(attribute: node)?.asDeclAttribute)
      case .OriginallyDefinedIn:
        return self.generateOriginallyDefinedInAttr(attribute: node).forEach { handle($0.asDeclAttribute) }
      case .PreInverseGenerics:
        return handle(self.generatePreInverseGenericsAttr(attribute: node)?.asDeclAttribute)
      case .PrivateImport:
        return handle(self.generatePrivateImportAttr(attribute: node)?.asDeclAttribute)
      case .ProjectedValueProperty:
        return handle(self.generateProjectedValuePropertyAttr(attribute: node)?.asDeclAttribute)
      case .RawLayout:
        return handle(self.generateRawLayoutAttr(attribute: node)?.asDeclAttribute)
      case .Section:
        return handle(self.generateSectionAttr(attribute: node)?.asDeclAttribute)
      case .Semantics:
        return handle(self.generateSemanticsAttr(attribute: node)?.asDeclAttribute)
      case .SILGenName:
        self.diagnoseNonLocalAttribute(node, attrName: attrName)
        return handle(self.generateSILGenNameAttr(attribute: node)?.asDeclAttribute)
      case .Specialize:
        return handle(self.generateSpecializeAttr(attribute: node, attrName: attrName)?.asDeclAttribute)
      case .Specialized:
        return handle(self.generateSpecializedAttr(attribute: node, attrName: attrName)?.asDeclAttribute)
      case .SPIAccessControl:
        return handle(self.generateSPIAccessControlAttr(attribute: node)?.asDeclAttribute)
      case .StorageRestrictions:
        return handle(self.generateStorageRestrictionAttr(attribute: node)?.asDeclAttribute)
      case .SwiftNativeObjCRuntimeBase:
        return handle(self.generateSwiftNativeObjCRuntimeBaseAttr(attribute: node)?.asDeclAttribute)
      case .Diagnose:
        return handle(self.generateDiagnoseAttr(attribute: node)?.asDeclAttribute)
      case .Target:
        return handle(self.generateTargetAttr(attribute: node)?.asDeclAttribute)
      case .Transpose:
        self.diagnoseNonLocalAttribute(node, attrName: attrName)
        return handle(self.generateTransposeAttr(attribute: node)?.asDeclAttribute)
      case .TypeEraser:
        return handle(self.generateTypeEraserAttr(attribute: node)?.asDeclAttribute)
      case .UnavailableFromAsync:
        return handle(self.generateUnavailableFromAsyncAttr(attribute: node)?.asDeclAttribute)
      case .Unsafe:
        return handle(self.generateUnsafeAttr(attribute: node)?.asDeclAttribute)
      case .Reasync:
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .AtReasync))
      case .Rethrows:
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .AtRethrows))
      case .Concurrent:
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .Concurrent))
      case .Once:
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .Once))
      case .Callable:
        return handle(self.generateCallableAttr(attribute: node)?.asDeclAttribute)
      case .ScopeParams:
        return handle(self.generateScopeParamsAttr(attribute: node)?.asDeclAttribute)
      case .Coroutine:
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .Coroutine))
      case nil where attrName == "_unavailableInEmbedded":
        return handle(self.generateUnavailableInEmbeddedAttr(attribute: node)?.asDeclAttribute)

      // Renamed attributes. These keep working, with a diagnostic whose
      // severity matches the C++ parser's.
      case nil where attrName == "_functionBuilder":
        self.diagnoseRenamedAttribute(node, to: "resultBuilder", warning: true)
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .ResultBuilder))
      case nil where attrName == "inlineable":
        self.diagnoseRenamedAttribute(node, to: "inlinable", closeMatch: true)
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .Inlinable))
      case nil where attrName == "_inlineable":
        self.diagnoseRenamedAttributeByLanguageMode(node, to: "inlinable")
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .Inlinable))
      case nil where attrName == "_versioned":
        self.diagnoseRenamedAttributeByLanguageMode(node, to: "usableFromInline")
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .UsableFromInline))
      case nil where attrName == "propertyDelegate" || attrName == "_propertyWrapper":
        self.diagnoseRenamedAttribute(node, to: "propertyWrapper", warning: true)
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .PropertyWrapper))
      case nil where attrName == "_section":
        self.diagnoseRenamedAttribute(node, to: "section", warning: true)
        return handle(self.generateSectionAttr(attribute: node)?.asDeclAttribute)
      case nil where attrName == "_used":
        self.diagnoseRenamedAttribute(node, to: "used", warning: true)
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .Used))
      case nil where attrName == "_predatesConcurrency":
        self.diagnoseRenamedAttribute(node, to: "preconcurrency", warning: true)
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .Preconcurrency))
      case nil where attrName == "warn":
        self.diagnoseRenamedAttribute(node, to: "diagnose", warning: true)
        return handle(self.generateDiagnoseAttr(attribute: node)?.asDeclAttribute)

      // Removed attributes. The C++ parser diagnoses and drops them.
      case nil where attrName == "_unsafeSendable" || attrName == "_unsafeMainActor":
        self.diagnose(
          .warn_attr_unsafe_removed(String(syntaxText: attrName)),
          at: node.atSign,
          fixIts: [self.tidyRemovalFixIt(node)]
        )
        return
      case nil where attrName == "warn_unused_result":
        // The behavior is now the default, so drop the attribute, with its
        // arguments if it has any.
        self.diagnose(
          .attr_warn_unused_result_removed,
          at: node.atSign,
          fixIts: [self.tidyRemovalFixIt(node)]
        )
        return

      // Simple attributes.
      case .AddressableSelf,
        .AddressableForDependencies,
        .AlwaysEmitConformanceMetadata,
        .AlwaysEmitIntoClient,
        .AtReasync,
        .AtRethrows,
        .Borrowed,
        .CompilerInitialized,
        .ConstVal,
        .ConstInitialized,
        .DynamicCallable,
        .EagerMove,
        .Exported,
        .DiscardableResult,
        .DisfavoredOverload,
        .DynamicMemberLookup,
        .EmitAssemblyVisionRemarks,
        .ExtractConstantsFromMembers,
        .FixedLayout,
        .Frozen,
        .GKInspectable,
        .GlobalActor,
        .HasInitialValue,
        .HasMissingDesignatedInitializers,
        .HasStorage,
        .IBAction,
        .IBDesignable,
        .IBInspectable,
        .IBOutlet,
        .IBSegueAction,
        .ImplementationOnly,
        .ImplicitSelfCapture,
        .InheritsConvenienceInitializers,
        .Inlinable,
        .Isolated,
        .LexicalLifetimes,
        .LLDBDebuggerFunction,
        .MainType,
        .Marker,
        .MoveOnly,
        .NeverEmitIntoClient,
        .NoAllocation,
        .NoDerivative,
        .NoEagerMove,
        .NoExistentials,
        .NoRuntime,
        .NoImplicitCopy,
        .NoLocks,
        .NoManualOwnership,
        .NoMetadata,
        .NoObjCBridging,
        .NonEphemeral,
        .NonEscapable,
        .NonObjC,
        .NonOverride,
        .NSApplicationMain,
        .NSCopying,
        .NSManaged,
        .ObjCMembers,
        .ObjCNonLazyRealization,
        .Owned,
        .Preconcurrency,
        .PropertyWrapper,
        .Reparentable,
        .RequiresStoredPropertyInits,
        .ResultBuilder,
        .Safe,
        .Sendable,
        .Sensitive,
        .SPIOnly,
        .ShowInInterface,
        .SpecializeExtension,
        .StaticExclusiveOnly,
        .Testable,
        .Transparent,
        .UIApplicationMain,
        .UnsafeInheritExecutor,
        .UnsafeNoObjCTaggedPointer,
        .UnsafeNonEscapableResult,
        .UsableFromInline,
        .Used,
        .WarnUnqualifiedAccess,
        .WeakLinked,
        .UnsafeSelfDependentResult:

        return handle(self.generateSimpleDeclAttr(attribute: node, kind: attrKind!))

      // Modifers.
      case .AccessControl:
        // TODO: Diagnose and generateAccessControl().
        fatalError("unimplemented (access control modifier parsed as attributes)")
      case .Nonisolated:
        // TODO: Diagnose.
        return handle(self.generateNonisolatedAttr(attribute: node)?.asDeclAttribute)
      case .ReferenceOwnership:
        // TODO: Diagnose.
        return handle(self.generateReferenceOwnershipAttr(attribute: node, attrName: attrName)?.asDeclAttribute)
      case .InheritActorContext:
        return handle(self.generateInheritActorContextAttr(attribute: node)?.asDeclAttribute)

      case .Async,
        .Consuming,
        .Borrowing,
        .Actor,
        .DistributedActor,
        .Required,
        .Optional,
        .Lazy,
        .Dynamic,
        .Infix,
        .Prefix,
        .Postfix,
        .LegacyConsuming,
        .Mutating,
        .NonMutating,
        .Convenience,
        .Override,
        .Indirect,
        .Final,
        .KnownToBeLocal,
        .CompileTimeLiteral:

        // generateSimpleDeclAttr will diagnose and fix-it to change it to modifiers.
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: attrKind!))

      // 'RejectByParser', these attribute kind should not be parsed as built-in attributes.
      case .RawDocComment,
        .ObjCBridged,
        .SynthesizedProtocol,
        .StaticInitializeObjCMetadata,
        .RestatedObjCConformance,
        .ClangImporterSynthesizedType,
        .ForbidSerializingReference,
        .Custom,
        .SetterAccess:
        assert(BridgedDeclAttribute.shouldBeRejectedByParser(attrKind!))
        // Fall back to CustomAttr.
        break

      case .none:
        // '@_accessorBlock' is a parser only disambiguation marker, ignore.
        if attrName == "_accessorBlock" {
          return
        }

        // Fall back to CustomAttr.
        break
      }
    }

    return handle(self.generateCustomAttr(attribute: node)?.asDeclAttribute)
  }

  /// Diagnose an attribute that does not make sense in a local scope.
  func diagnoseNonLocalAttribute(_ node: AttributeSyntax, attrName: SyntaxText) {
    if self.declContext.isLocalContext {
      self.diagnose(.attr_name_only_at_non_local_scope("@" + String(syntaxText: attrName)), at: node.attributeName)
    }
  }

  /// The range of `node` from its `@` through its last token, excluding trivia.
  func generateCharSourceRange(attribute node: AttributeSyntax) -> CharSourceRange {
    self.generateCharSourceRange(
      start: node.atSign.positionAfterSkippingLeadingTrivia,
      length: SourceLength(utf8Length: node.endPositionBeforeTrailingTrivia.utf8Offset
        - node.atSign.positionAfterSkippingLeadingTrivia.utf8Offset)
    )
  }

  /// Diagnose the use of an old spelling of an attribute, replacing it with
  /// `newName`.
  func diagnoseRenamedAttribute(
    _ node: AttributeSyntax,
    to newName: String,
    warning: Bool = false,
    closeMatch: Bool = false
  ) {
    let oldName = node.attributeName.trimmedDescription
    let diagnostic: CompilerDiagnostic =
      if closeMatch {
        .attr_name_close_match(oldName, newName)
      } else if warning {
        .attr_renamed_warning(oldName, newName)
      } else {
        .attr_renamed(oldName, newName)
      }
    self.diagnose(
      diagnostic,
      at: node.attributeName,
      fixIts: [.init(replace: self.generateCharSourceRange(node.attributeName), with: newName)]
    )
  }

  /// `@_versioned` and `@_inlineable` are errors in Swift 5 and later, warnings
  /// in Swift 4.2, and silently accepted before that.
  func diagnoseRenamedAttributeByLanguageMode(_ node: AttributeSyntax, to newName: String) {
    let langOpts = self.ctx.langOpts
    if langOpts.isLanguageModeAtLeast(major: 5, minor: 0) {
      self.diagnoseRenamedAttribute(node, to: newName)
    } else if langOpts.isLanguageModeAtLeast(major: 4, minor: 2) {
      self.diagnoseRenamedAttribute(node, to: newName, warning: true)
    }
  }

  /// Diagnose an attribute that requires an argument clause but does not have a
  /// usable one.
  ///
  /// Matches the C++ parser, which points at the attribute name rather than at
  /// the `@`.
  func diagnoseExpectedAttributeArguments(_ node: AttributeSyntax) {
    self.diagnose(
      .attr_expected_lparen(node.attributeName.trimmedDescription, /*isModifier=*/false),
      at: node.attributeName
    )
  }

  /// E.g.:
  ///   ```
  ///   @abi(func fn())
  ///   ```
  func generateABIAttr(attribute node: AttributeSyntax) -> BridgedABIAttr? {
    guard
      let arg = node.arguments?.as(ABIAttributeArgumentsSyntax.self)
    else {
      // TODO: diagnose
      return nil
    }

    let abiDecl: BridgedDecl?
    switch arg.provider {
    case .associatedType(let assocTyDecl):
      abiDecl = self.generate(associatedTypeDecl: assocTyDecl)?.asDecl
    case .deinitializer(let deinitDecl):
      abiDecl = self.generate(deinitializerDecl: deinitDecl).asDecl
    case .enumCase(let caseDecl):
      abiDecl = self.generate(enumCaseDecl: caseDecl).asDecl
    case .function(let funcDecl):
      abiDecl = self.generate(functionDecl: funcDecl)?.asDecl
    case .initializer(let initDecl):
      abiDecl = self.generate(initializerDecl: initDecl).asDecl
    case .`subscript`(let subscriptDecl):
      abiDecl = self.generate(subscriptDecl: subscriptDecl).asDecl
    case .typeAlias(let typealiasDecl):
      abiDecl = self.generate(typeAliasDecl: typealiasDecl)?.asDecl
    case .variable(let varDecl):
      abiDecl = self.generate(variableDecl: varDecl, isABIOnly: true)
    case .missing(_):
      // This error condition will have been diagnosed in SwiftSyntax.
      abiDecl = nil
    }

    // TODO: Diagnose if `abiDecl` has a body/initial value/etc.
    // The C++ parser considers it syntactically invalid but SwiftSyntax does not.

    // Without an inner declaration there is nothing for the attribute to name, and
    // `ABIAttr::attachToDeclImpl` unconditionally inspects it. The C++ parser also
    // creates no attribute in this case; whatever made the declaration
    // unrecoverable has already been diagnosed.
    guard abiDecl != nil else {
      return nil
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      abiDecl: abiDecl.asNullable
    )
  }

  /// E.g.:
  ///   ```
  ///   @_alignment(8)
  ///   ```
  func generateAlignmentAttr(attribute node: AttributeSyntax) -> BridgedAlignmentAttr? {
    self.generateWithLabeledExprListArguments(attribute: node) { args in
      let value: Int? = self.generateConsumingAttrOption(args: &args, label: nil) { expr in
        guard let intExpr = expr.as(IntegerLiteralExprSyntax.self) else {
          return nil
        }
        return intExpr.representedLiteralValue
      }
      // Zero is accepted here; Sema diagnoses a value that is not a power of two.
      guard let value, value >= 0, value <= Int(UInt32.max) else {
        self.diagnose(.alignment_must_be_positive_integer, at: node.attributeName)
        return nil
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        value: value
      )
    }
  }

  /// E.g.:
  ///   ```
  ///   @_allowFeatureSuppression(IsolatedAny)
  ///   ```
  func generateAllowFeatureSuppressionAttr(attribute node: AttributeSyntax, attrName: SyntaxText) -> BridgedAllowFeatureSuppressionAttr? {
    guard case .argumentList(let args) = node.arguments
    else {
      // TODO: Diagnose.
      return nil
    }

    let inverted: Bool
    switch attrName {
    case "_allowFeatureSuppression":
      inverted = false
    case "_disallowFeatureSuppression":
      inverted = true
    default:
      return nil
    }

    let features = args.compactMap(in: self) { arg -> Identifier? in
      guard arg.label == nil,
            let declNameExpr = arg.expression.as(DeclReferenceExprSyntax.self),
            declNameExpr.argumentNames == nil,
            declNameExpr.moduleSelector == nil
      else {
        // TODO: Diagnose.
        return nil
      }

      return generateIdentifier(declNameExpr.baseName)
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      inverted: inverted,
      features: features)
  }

  /// E.g.:
  ///   ```
  ///   @available(macOS 10.12, iOS 13, *)
  ///   @available(macOS, introduced: 10.12)
  ///   ```
  func generateAvailableAttr(attribute node: AttributeSyntax, attrName: SyntaxText) -> [BridgedAvailableAttr] {
    guard let args = node.arguments else {
      self.diagnoseExpectedAttributeArguments(node)
      return []
    }
    guard let args = args.as(AvailabilityArgumentListSyntax.self) else {
      // TODO: Diagnose.
      return []
    }

    return self.generateAvailableAttr(
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      attrName: attrName,
      args: args
    )
  }

  /// E.g.:
  ///   ```
  ///   @backDeployed(before: SwiftStdlib 5.9)
  ///   ```
  func generateBackDeployedAttr(attribute node: AttributeSyntax) -> [BridgedBackDeployedAttr] {
    guard let args = node.arguments else {
      self.diagnoseExpectedAttributeArguments(node)
      return []
    }
    guard let args = args.as(BackDeployedAttributeArgumentsSyntax.self) else {
      // TODO: Diagnose.
      return []
    }

    let atLoc = self.generateSourceLoc(node.atSign)
    let range = self.generateAttrSourceRange(node)

    let platformVersions = self.generate(
      platformVersionList: args.platforms,
      attrName: "@backDeployed"
    )
    if platformVersions.hadError {
      return []
    }
    if platformVersions.needsPlatformVersionDiagnostic {
      self.diagnose(.attr_availability_need_platform_version("@backDeployed"), at: node.attributeName)
      return []
    }
    var result: [BridgedBackDeployedAttr] = []
    for platformVersion in platformVersions.versions {
      let attr = BridgedBackDeployedAttr.createParsed(
        ctx,
        atLoc: atLoc,
        range: range,
        platform: platformVersion.platform,
        version: platformVersion.version
      )
      result.append(attr)
    }
    return result
  }

  /// E.g.:
  ///   ```
  ///   @_cdecl("c_function_name")
  ///   @cdecl(c_function_name)
  ///   @cdecl
  ///   ```
  func generateCDeclAttr(attribute node: AttributeSyntax) -> BridgedCDeclAttr? {
    let attrName = node.attributeName.as(IdentifierTypeSyntax.self)?.name.text
    let underscored = attrName?.hasPrefix("_") ?? false

    var name: BridgedStringRef? = nil
    if node.arguments != nil || underscored {
      name = self.generateWithLabeledExprListArguments(attribute: node) {
          args in
        if underscored {
          self.generateConsumingSimpleStringLiteralAttrOption(args: &args)
        } else {
           self.generateConsumingPlainIdentifierAttrOption(args: &args) {
             return $0.rawText.bridged
           }
        }
      }
      guard name != nil else {
        // `@c("name")`, `@c()` and `@c(42)` have well-formed arguments of the
        // wrong kind; anything else was diagnosed by the parser.
        if !underscored, !node.hasError, let attrName {
          self.diagnose(.attr_expected_cname(attrName), at: node.attributeName)
        }
        return nil
      }
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      // No name must be a *null* string, not an empty one: Sema treats an
      // empty-but-non-null name of a `@c` function as an explicit (empty)
      // selector, which then never matches the header's declaration.
      name: name ?? BridgedStringRef(),
      underscored: underscored
    )
  }

  func generateCxxDeclAttr(attribute node: AttributeSyntax) -> BridgedCxxDeclAttr? {
    // The optional identifier argument is the C++ function name the importer
    // matches against.
    var name: BridgedStringRef = ""
    if let arguments = node.arguments {
      // A C++ operator name written without backticks is not a valid argument.
      if arguments.trimmedDescription.hasPrefix("operator"),
        !arguments.trimmedDescription.hasPrefix("operator:")
      {
        self.diagnose(.attr_cxx_operator_name_backticks("cxx"), at: node.attributeName)
        return nil
      }
      // The name is a single bare identifier: not empty, not a string literal,
      // not labeled, and not a keyword.
      let isPlainName: Bool = {
        guard let list = arguments.as(LabeledExprListSyntax.self), list.count == 1,
          let first = list.first, first.label == nil,
          let reference = first.expression.as(DeclReferenceExprSyntax.self),
          reference.argumentNames == nil,
          reference.baseName.rawTokenKind == .identifier
        else {
          return false
        }
        return true
      }()
      guard isPlainName else {
        self.diagnose(.attr_expected_cxx_name("cxx"), at: node.attributeName)
        return nil
      }
      guard let parsed = self.generateWithLabeledExprListArguments(attribute: node, { args in
        self.generateConsumingPlainIdentifierAttrOption(args: &args) { (token) -> BridgedStringRef? in
          var text = token.rawText
          if text.count > 2 && text.hasPrefix("`") && text.hasSuffix("`") {
            text = .init(rebasing: text.dropFirst().dropLast())
          }
          return text.bridged
        }
      }) else {
        return nil
      }
      name = parsed
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      name: name
    )
  }

  struct GeneratedDerivativeOriginalDecl {
    var baseType: BridgedTypeRepr?
    var declName: BridgedDeclNameRef
    var declNameLoc: BridgedDeclNameLoc
  }

  func generateDerivativeOriginalDecl(expr: ExprSyntax) -> GeneratedDerivativeOriginalDecl? {
    var baseType: BridgedTypeRepr?
    var declName: BridgedDeclNameRef
    var declNameLoc: BridgedDeclNameLoc

    if let declrefExpr = expr.as(DeclReferenceExprSyntax.self) {
      baseType = nil
      (declName, declNameLoc) =  self.generateDeclNameRef(declReferenceExpr: declrefExpr)

    } else if let memberExpr = expr.as(MemberAccessExprSyntax.self),
              let baseExpr = memberExpr.base {
      guard let _baseType = self.generateTypeRepr(expr: baseExpr) else {
        // TODO: Diagnose.
        fatalError("invalid type expression for @derivative qualified decl name")
      }
      baseType = _baseType
      (declName, declNameLoc) = self.generateDeclNameRef(declReferenceExpr: memberExpr.declName)

    } else if let typeExpr = expr.as(TypeExprSyntax.self),
      let identifierType = typeExpr.type.as(IdentifierTypeSyntax.self),
      identifierType.genericArgumentClause == nil
    {
      // SwiftParser parses a bare name followed by an accessor label
      // (`of: property.set`) as a type.
      baseType = nil
      (declName, declNameLoc) = self.generateDeclNameRef(
        moduleSelector: identifierType.moduleSelector,
        baseName: identifierType.name
      )

    } else if let typeExpr = expr.as(TypeExprSyntax.self),
      let memberType = typeExpr.type.as(MemberTypeSyntax.self),
      memberType.genericArgumentClause == nil
    {
      // `of: Base.property.set`
      baseType = self.generate(type: memberType.baseType)
      (declName, declNameLoc) = self.generateDeclNameRef(
        moduleSelector: nil,
        baseName: memberType.name
      )

    } else {
      // TODO: Diagnose.
      fatalError("invalid expression for @derivative original decl name")
    }

    return GeneratedDerivativeOriginalDecl(
      baseType: baseType,
      declName: declName,
      declNameLoc: declNameLoc
    )
  }

  func generateDifferentiabilityKind(text: SyntaxText) -> BridgedDifferentiabilityKind {
    switch text {
    case "reverse": return .reverse
    case "wrt", "withRespectTo": return .normal
    case "_linear": return .linear
    case "_forward": return .forward
    default: return .nonDifferentiable
    }
  }

  func generate(differentiabilityArgument node: DifferentiabilityArgumentSyntax) -> BridgedParsedAutoDiffParameter {
    let loc = self.generateSourceLoc(node)
    switch node.argument.rawTokenKind {
    case .identifier:
      return .forNamed(self.generateIdentifier(node.argument), loc: loc)

    case .integerLiteral:
      guard let index = Int(node.argument.text) else {
        // TODO: Diagnose
        fatalError("(compiler bug) invalid integer literal token text")
      }
      return .forOrdered(index, loc: loc)

    case .keyword where node.argument.rawText == "self":
      return .forSelf(loc: loc)

    default:
      // TODO: Diagnose
      fatalError("(compiler bug) invalid token for 'wrt:' argument")
    }
  }

  func generate(differentiabilityWithRespectToArgument node: DifferentiabilityWithRespectToArgumentSyntax?) -> BridgedArrayRef {
    guard let node else {
      return BridgedArrayRef()
    }
    switch node.arguments {
    case .argument(let node): // Single argument e.g. 'wrt: foo'
      return CollectionOfOne(self.generate(differentiabilityArgument: node)).bridgedArray(in: self)
    case .argumentList(let node): // Multiple arguments e.g. 'wrt: (self, 2)'
      return  node.arguments.lazy.map(self.generate(differentiabilityArgument:)).bridgedArray(in: self)
    }
  }

  /// E.g.:
  ///   ```
  ///   @com
  ///   @com(interface: "...")
  ///   @com(implementation: "...")
  ///   @com(implementation: "...", threading: "...")
  ///   ```
  func generateCOMAttr(attribute node: AttributeSyntax) -> BridgedCOMAttr? {
    let location = generateSourceLoc(node.atSign)
    let range = generateAttrSourceRange(node)

    if node.arguments == nil {
      return .createParsed(ctx, atLoc: location, range: range, interface: "",
                           implementation: BridgedStringRef(), threading:
                           .apartment)
    }

    typealias Result =
        (interface: BridgedStringRef,
         implementation: BridgedStringRef,
         threading: BridgedCOMThreadingModel)

    guard let parsed =
        generateWithLabeledExprListArguments(attribute: node, { arguments -> Result? in
          var interface: BridgedStringRef = ""
          var implementation: BridgedStringRef = BridgedStringRef()
          var threading: BridgedCOMThreadingModel? = .apartment

          if arguments.isEmpty {
            self.diagnose(.attr_com_expected_label, at: node.attributeName)
            return nil
          }

          for argument in arguments {
            switch argument.label?.rawText {
            case let label where label == "interface":
              guard let value =
                  generateConsumingSimpleStringLiteralAttrOption(args: &arguments,
                                                                 label: label) else {
                    self.diagnose(.attr_expected_string_literal("com"), at: node.attributeName)
                    return nil
                  }
              interface = value
            case let label where label == "implementation":
              guard let value =
                  generateConsumingSimpleStringLiteralAttrOption(args: &arguments,
                                                                 label: label) else {
                    self.diagnose(.attr_expected_string_literal("com"), at: node.attributeName)
                    return nil
                  }
              implementation = value
            case "threading":
              // Consume the labelled argument manually.
              arguments.removeFirst()
              threading =
                  switch argument.expression.as(MemberAccessExprSyntax.self)?
                            .declName.baseName.rawText {
                  case "single"?: .single
                  case "apartment"?, "sta"?: .apartment
                  case "free"?, "mta"?: .free
                  case "both"?: .both
                  case "neutral"?: .neutral
                  default: nil
                  }
              if threading == nil {
                if let member = argument.expression.as(MemberAccessExprSyntax.self) {
                  self.diagnose(
                    .attr_com_unknown_threading_model(member.declName.baseName.text),
                    at: node.attributeName)
                } else {
                  self.diagnose(.attr_com_expected_threading_model, at: node.attributeName)
                }
              }
            case nil:
              self.diagnose(.attr_com_expected_label, at: node.attributeName)
              return nil
            case let label?:
              self.diagnose(.attr_com_unknown_label(String(syntaxText: label)), at: node.attributeName)
              return nil
            }
          }

          // TODO: diagnose
          guard let threading else { return nil }
          return Result(interface: interface, implementation: implementation,
                        threading: threading)
        }) else {
      return nil
    }

    return .createParsed(ctx, atLoc: location, range: range,
                         interface: parsed.interface,
                         implementation: parsed.implementation,
                         threading: parsed.threading)
  }

  /// E.g.
  ///   ```
  ///   @derivative(of: foo(arg:), wrt: self)
  ///   ```
  /// The accessor kind that a label such as `get` in `@derivative(of: p.get)`
  /// names.
  static func accessorKind(forLabel label: String) -> swift.AccessorKind? {
    switch label {
    case "get": return .get
    case "set": return .set
    case "willSet": return .willSet
    case "didSet": return .didSet
    case "_read": return ._read
    case "_modify": return ._modify
    case "unsafeAddress": return .unsafeAddress
    case "unsafeMutableAddress": return .unsafeMutableAddress
    default: return nil
    }
  }

  func generateDerivativeAttr(attribute node: AttributeSyntax) -> BridgedDerivativeAttr? {
    guard let args = node.arguments?.as(DerivativeAttributeArgumentsSyntax.self) else {
      fatalError("(compiler bug) invalid arguments for @derivative attribute")
    }
    // SwiftParser may leave an accessor label inside the original name
    // (`of: property.get`), where the C++ parser takes a final `get`/`set`/...
    // as the accessor kind of what comes before it.
    var originalName = args.originalDeclName
    var accessorKind: swift.AccessorKind? = nil
    if args.accessorSpecifier == nil, let member = originalName.as(MemberAccessExprSyntax.self),
      let base = member.base, member.declName.argumentNames == nil,
      let kind = Self.accessorKind(forLabel: member.declName.baseName.text)
    {
      originalName = base
      accessorKind = kind
    }
    guard let originalDecl = self.generateDerivativeOriginalDecl(expr: originalName) else {
      return nil
    }

    if let accessorToken = args.accessorSpecifier {
      accessorKind = self.generate(
        accessorSpecifier: accessorToken,
        modifiers: []
      )
    }

    let parameters = self.generate(differentiabilityWithRespectToArgument: args.arguments)

    if let accessorKind {
      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        baseType: originalDecl.baseType.asNullable,
        originalName: originalDecl.declName,
        originalNameLoc: originalDecl.declNameLoc,
        accessorKind: accessorKind,
        params: parameters
      )
    } else {
      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        baseType: originalDecl.baseType.asNullable,
        originalName: originalDecl.declName,
        originalNameLoc: originalDecl.declNameLoc,
        params: parameters
      )
    }
  }


  /// E.g.
  ///   ```
  ///   @differentiable(reverse, wrt: (self, 3) where T: U)
  ///   @differentiable(reverse, wrt: foo where T: U)
  ///   ```
  func generateDifferentiableAttr(attribute node: AttributeSyntax) -> BridgedDifferentiableAttr? {
    guard let args = node.arguments?.as(DifferentiableAttributeArgumentsSyntax.self) else {
      fatalError("(compiler bug) invalid arguments for @differentiable attribute")
    }

    var differentiability: BridgedDifferentiabilityKind
    if let kindSpecifier = args.kindSpecifier {
      differentiability = self.generateDifferentiabilityKind(text: kindSpecifier.rawText)
    } else {
      differentiability = .normal
    }
    if differentiability == .normal {
      // TODO: Diagnose "'@differentiable' has been renamed to '@differentiable(reverse)"
      differentiability = .reverse
    }
    guard differentiability == .reverse || differentiability == .linear else {
      // E.g. '@differentiable(_forward)'. Only 'reverse' (and the SIL-only
      // '_linear') are supported.
      self.diagnose(
        .attr_differentiable_kind_not_supported(
          args.kindSpecifier.map { String(syntaxText: $0.rawText) } ?? ""
        ),
        at: args.kindSpecifier.map(Syntax.init) ?? Syntax(node.attributeName)
      )
      return nil
    }

    let parameters = self.generate(differentiabilityWithRespectToArgument: args.arguments)

    let whereClause: BridgedTrailingWhereClause?
    if let node = args.genericWhereClause {
      whereClause = self.generate(genericWhereClause: node)
    } else {
      whereClause = nil
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      kind: differentiability,
      params: parameters,
      genericWhereClause: whereClause.asNullable
    )
  }

  /// E.g:
  ///   ```
  ///   @_dynamicReplacement(for: member)
  ///   ```
  func generateDynamicReplacementAttr(attribute node: AttributeSyntax) -> BridgedDynamicReplacementAttr? {
    guard
      // `@_dynamicReplacement` has special argument list syntax
      let arg = node.arguments?.as(DynamicReplacementAttributeArgumentsSyntax.self)
    else {
      // TODO: Diagnose
      return nil
    }

    let replacedFunction = self.generateDeclNameRef(declReferenceExpr: arg.declName)

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      attrNameLoc: self.generateSourceLoc(node.attributeName),
      lParenLoc: self.generateSourceLoc(node.leftParen),
      replacedFunction: replacedFunction.name,
      replacedFunctionLoc: replacedFunction.loc,
      rParenLoc: self.generateSourceLoc(node.rightParen)
    )
  }

  /// E.g.:
  ///  ```
  ///  @_documentation(visibility: internal)
  ///  @_documentation(metadata: foobar)
  ///  @_documentation(metadata: "longer string")
  ///  ```
  func generateDocumentationAttr(attribute node: AttributeSyntax) -> BridgedDocumentationAttr? {
    guard var args = node.arguments?.as(DocumentationAttributeArgumentListSyntax.self)?[...] else {
      // TODO: Diagnose
      return nil
    }

    var visibility: swift.AccessLevel?
    var metadata: BridgedStringRef? = nil

    while let arg = args.popFirst() {
      switch arg.label.rawText {
      case "visibility":
        guard visibility == nil else {
          // TODO: Diagnose duplicated 'visibility" arguments
          continue
        }
        guard case .token(let token) = arg.value else {
          // TODO: Diagnose
          continue
        }
        switch token.keywordKind {
        case .open: visibility = .open
        case .public: visibility = .public
        case .package: visibility = .package
        case .internal: visibility = .internal
        case .private: visibility = .private
        case .fileprivate: visibility = .fileprivate
        default:
          // TODO: Diagnose
          continue
        }
      case "metadata":
        guard metadata == nil else {
          // TODO: Diagnose duplicated 'metadata" arguments
          continue
        }
        switch arg.value {
        case .string(let str):
          guard let text = self.generateStringLiteralTextIfNotInterpolated(expr: str) else {
            // TODO: Diagnose
            continue
          }
          metadata = text
        case .token(let tok) where tok.rawTokenKind == .identifier:
          metadata = tok.rawText.bridged
        default:
          // TODO: Diagnose
          continue
        }
      default:
        // TODO: Diagnose invalid argument
        continue;
      }
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      metadata: metadata ?? "",
      accessLevel: visibility == nil ? .init() : .init(visibility!)
    )
  }

  /// E.g.:
  ///   ```
  ///   @effects(readonly)
  ///   @effects(customKind foo.bar)
  ///   ```
  func generateEffectsAttr(attribute node: AttributeSyntax) -> BridgedEffectsAttr? {
    guard
      let arguments = node.arguments?.as(EffectsAttributeArgumentListSyntax.self),
      arguments.count >= 1
    else {
      // TODO: Diagnose?
      return nil
    }
    let effectKind: swift.EffectsKind
    switch arguments.first!.rawText {
    case "readonly":
      effectKind = .readonly
    case "readnone":
      effectKind = .readnone
    case "releasenone":
      effectKind = .releasenone
    case "readwrite":
      effectKind = .readwrite
    default:
      effectKind = .custom
    }

    if effectKind != .custom {
      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        effectKind: effectKind
      )
    } else {
      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        customString: self.extractRawText(arguments).bridged,
        customStringLoc: self.generateSourceLoc(arguments)
      )
    }
  }

  /// E.g.
  ///   ```
  ///   @exclusivity(unchecked)
  ///   @exclusivity(checked)
  ///   ```
  /// E.g.:
  ///   ```
  ///   @nonexhaustive
  ///   @nonexhaustive(warn)
  ///   ```
  func generateNonexhaustiveAttr(attribute node: AttributeSyntax) -> BridgedNonexhaustiveAttr? {
    let mode: BridgedNonexhaustiveMode? = self.generateSingleAttrOption(
      attribute: node,
      options: ["warn"],
      {
        switch $0.rawText {
        case "warn": return .warning
        default: return nil
        }
      },
      valueIfOmitted: .error
    )
    guard let mode else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      mode: mode
    )
  }

  func generateExclusivityAttr(attribute node: AttributeSyntax) -> BridgedExclusivityAttr? {
    let mode: BridgedExclusivityAttrMode? = self.generateSingleAttrOption(
      attribute: node,
      options: ["checked", "unchecked"],
      {
        switch $0.rawText {
        case "checked": return .checked
        case "unchecked": return .unchecked
        default: return nil
        }
      }
    )
    guard let mode else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      mode: mode
    )
  }

  /// E.g.
  ///   ```
  ///   @_expose(Cxx)
  ///   @_expose(Cxx, "cxx_name")
  ///   @_expose(Wasm, "wasm_name")
  ///   ```
  func generateExposeAttr(attribute node: AttributeSyntax) -> BridgedExposeAttr? {
    // FIXME: SwiftParser should parse the argument as LabeledExprListArguments
    return self.generateWithLabeledExprListArguments(attribute: node) { args in
      // Exposure kind: 'Cxx', '!Cxx' or 'wasm'.
      let optionLoc: Syntax = args.first.map { Syntax($0) } ?? node.rightParen.map { Syntax($0) } ?? Syntax(node.attributeName)
      let kind: swift.ExposureKind? = self.generateConsumingAttrOption(args: &args, label: nil) { expr in
        var option = expr
        var isNegated = false
        if let prefix = expr.as(PrefixOperatorExprSyntax.self), prefix.operator.text == "!" {
          isNegated = true
          option = prefix.expression
        }
        guard let declRef = option.as(DeclReferenceExprSyntax.self), declRef.argumentNames == nil else {
          return nil
        }
        switch (declRef.baseName.text, isNegated) {
        case ("Cxx", false): return .cxx
        case ("Cxx", true): return .notcxx
        case ("wasm", false): return .wasm
        default: return nil
        }
      }
      guard let kind else {
        self.diagnose(.attr_expected_option_such_as("_expose", "Cxx"), at: optionLoc)
        return nil
      }

      // A declaration cannot have two '@_expose' attributes with the same
      // exposure kind.
      if self.seenExposureKinds.contains(kind.rawValue) {
        self.diagnose(.duplicate_attribute(false), at: node.attributeName)
      }
      self.seenExposureKinds.append(kind.rawValue)

      // Name.
      let name: BridgedStringRef?
      if !args.isEmpty {
        name = self.generateConsumingSimpleStringLiteralAttrOption(args: &args) ?? ""
        guard name != nil else {
          return nil
        }
      } else {
        name = nil
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        name: name ?? BridgedStringRef(),
        kind: kind
      )
    }
  }

  /// E.g.
  ///   ```
  ///   @_extern(c)
  ///   @_extern(c, "c_name")
  ///   @_extern(wasm, module: "x", name: "y")
  ///   ```
  func generateExternAttr(attribute node: AttributeSyntax) -> BridgedExternAttr? {
    guard let arguments = node.arguments?.as(LabeledExprListSyntax.self) else {
      self.diagnoseExpectedAttributeArguments(node)
      return nil
    }

    // The arguments are checked token by token, as the C++ parser does, because
    // the diagnostics depend on exactly which token is missing.
    let attrName = node.attributeName.trimmedDescription
    let tokens = Array(arguments.tokens(viewMode: .sourceAccurate))
    var index = 0
    var current: TokenSyntax? { index < tokens.count ? tokens[index] : nil }
    var currentOrEnd: Syntax { current.map(Syntax.init) ?? Syntax(node.rightParen ?? node.attributeName.lastToken(viewMode: .sourceAccurate)!) }

    /// Parse `, [label:] "string"`; returns `nil` after diagnosing a problem.
    func parseStringArgument(field: String?) -> BridgedStringRef? {
      guard current?.tokenKind == .comma else {
        self.diagnose(.attr_expected_comma(attrName, false), at: node.attributeName)
        return nil
      }
      index += 1
      if let field {
        guard let label = current, label.rawTokenKind == .identifier, label.text == field else {
          self.diagnose(.attr_extern_expected_label(field), at: node.attributeName)
          return nil
        }
        index += 1
        guard current?.tokenKind == .colon else {
          self.diagnose(.attr_expected_colon_after_label(field), at: currentOrEnd)
          return nil
        }
        index += 1
      }
      guard let literal = current.flatMap({ Syntax($0).ancestorOrSelf(mapping: { $0.as(StringLiteralExprSyntax.self) }) }),
        literal.openingQuote.id == current?.id || literal.openingPounds?.id == current?.id
      else {
        self.diagnose(.attr_expected_string_literal(attrName), at: node.attributeName)
        return nil
      }
      // Skip the whole literal.
      let last = literal.lastToken(viewMode: .sourceAccurate)
      while let token = current, token.id != last?.id {
        index += 1
      }
      index += 1
      return self.generateStringLiteralTextIfNotInterpolated(expr: literal)
    }

    guard let kindToken = current, kindToken.rawTokenKind == .identifier else {
      self.diagnose(.attr_expected_option_such_as(attrName, "c"), at: currentOrEnd)
      return nil
    }
    index += 1

    var kind: swift.ExternKind
    var moduleName: BridgedStringRef?
    var symbolName: BridgedStringRef?
    var discard = false
    switch kindToken.text {
    case "wasm":
      kind = .wasm
      moduleName = parseStringArgument(field: "module")
      discard = moduleName == nil
      symbolName = parseStringArgument(field: "name")
      discard = discard || symbolName == nil
    case "c":
      kind = .c
      if current?.tokenKind == .comma {
        symbolName = parseStringArgument(field: nil)
        discard = symbolName == nil
      }
    default:
      self.diagnose(.attr_expected_option_such_as(attrName, "c"), at: currentOrEnd)
      return nil
    }

    // Anything left over is not the closing parenthesis.
    if current != nil {
      self.diagnose(.attr_expected_rparen(attrName, false), at: node.attributeName)
      return nil
    }

    // A declaration cannot have two '@_extern' attributes of the same kind.
    if !discard {
      if self.seenExternKinds.contains(kind.rawValue) {
        self.diagnose(.duplicate_attribute(false), at: node.attributeName)
        discard = true
      }
      self.seenExternKinds.append(kind.rawValue)
    }
    if discard {
      return nil
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      lParenLoc: self.generateSourceLoc(node.leftParen),
      rParenLoc: self.generateSourceLoc(node.rightParen),
      kind: kind,
      moduleName: moduleName ?? BridgedStringRef(),
      name: symbolName ?? BridgedStringRef()
    )
  }

  /// E.g.
  ///   ```
  ///   @section("__TEXT,__mysection")
  ///   @section(default)
  ///   ```
  func generateSectionAttr(attribute node: AttributeSyntax) -> BridgedSectionAttr? {
    let isDefault: Bool
    let name: BridgedStringRef
    if let arg = node.arguments?.as(SectionAttributeArgumentSyntax.self) {
      switch arg.section {
      case .defaultKeyword:
        isDefault = true
        name = ""
      case .expression(let expr):
        guard let sectionName = self.generateStringLiteralTextIfNotInterpolated(expr: expr) else {
          return nil
        }
        isDefault = false
        name = sectionName
      }
    } else if let list = node.arguments?.as(LabeledExprListSyntax.self),
      list.count == 1, let first = list.first, first.label == nil
    {
      // The historical spelling '@_section' isn't known to swift-syntax, so
      // its arguments arrive as a generic expression list.
      guard let sectionName = self.generateStringLiteralTextIfNotInterpolated(expr: first.expression) else {
        return nil
      }
      isDefault = false
      name = sectionName
    } else {
      self.diagnoseExpectedAttributeArguments(node)
      return nil
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      isDefault: isDefault,
      name: name
    )
  }

  /// E.g.:
  ///   ```
  ///   @_implements(ProtocolName, member())
  ///   ```
  func generateImplementsAttr(attribute node: AttributeSyntax) -> BridgedImplementsAttr? {
    guard
      // `@_dynamicReplacement` has special argument list syntax
      let arg = node.arguments?.as(ImplementsAttributeArgumentsSyntax.self)
    else {
      // TODO: Diagnose
      return nil
    }

    if arg.declName.moduleSelector != nil {
      // TODO: Diagnose; module selector is meaningless at this position.
    }

    let type = self.generate(type: arg.type)
    let member = self.generateDeclNameRef(declReferenceExpr: arg.declName)

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      protocolType: type,
      memberName: member.name,
      memberNameLoc: member.loc
    )
  }

  /// E.g.:
  ///   ```
  ///   @export(interface)
  ///   @export(implementation)
  ///   ```
  func generateExportAttr(attribute node: AttributeSyntax) -> BridgedExportAttr? {
    let kind: swift.ExportKind? = self.generateSingleAttrOption(
      attribute: node,
      options: ["interface", "implementation"],
      {
        switch $0.rawText {
        case "interface": return .interface
        case "implementation": return .implementation
        default: return nil
        }
      }
    )
    guard let kind else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      kind: kind
    )
  }

  /// E.g.:
  ///   ```
  ///   @inline(never)
  ///   @inline(__always)
  ///   @inline(always)
  ///   ```
  func generateInlineAttr(attribute node: AttributeSyntax) -> BridgedInlineAttr? {
    let kind: swift.InlineKind? = self.generateSingleAttrOption(
      attribute: node,
      options: ["never", "always", "__always"],
      {
        switch $0.rawText {
        case "never": return .never
        case "__always": return .alwaysUnderscored
        case "always": return .always
        default: return nil
        }
      }
    )
    guard let kind else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      kind: kind
    )
  }

  func generateLifetimeDescriptor(nameToken node: TokenSyntax, lifetimeDependenceKind: BridgedParsedLifetimeDependenceKind = .default) -> BridgedLifetimeDescriptor {
    let ident = self.generateIdentifier(node)
    let loc = self.generateSourceLoc(node)
    if ident == ctx.id_self {
      return .forSelf(
        dependenceKind: lifetimeDependenceKind,
        loc: loc
      )
    } else {
      return .forNamed(
        ident,
        dependenceKind: lifetimeDependenceKind,
        loc: loc
      );
    }
  }

  /// Generate a lifetime descriptor, i.e. the `borrow x` in `@_lifetime(borrow x)`.
  ///
  /// `allowIndices` says whether a parameter index is accepted in this position,
  /// which only affects the wording of the diagnostic when it is not -- matching
  /// the C++ parser's `parseLifetimeDescriptor`.
  func generateLifetimeDescriptor(
    expr node: ExprSyntax,
    allowIndices: Bool
  ) -> BridgedLifetimeDescriptor? {
    let lifetimeDependenceKind: BridgedParsedLifetimeDependenceKind
    let descriptorExpr: ExprSyntax
    if let copyExpr = node.as(CopyExprSyntax.self) {
      lifetimeDependenceKind = .inherit
      descriptorExpr = copyExpr.expression
    } else if let borrowExpr = node.as(BorrowExprSyntax.self) {
      lifetimeDependenceKind = .borrow
      descriptorExpr = borrowExpr.expression
    } else if let inoutExpr = node.as(InOutExprSyntax.self) {
      lifetimeDependenceKind = .inout
      descriptorExpr = inoutExpr.expression
    } else {
      lifetimeDependenceKind = .default
      descriptorExpr = node
    }

    let loc = self.generateSourceLoc(descriptorExpr)
    if
      let declRefExpr = descriptorExpr.as(DeclReferenceExprSyntax.self),
      declRefExpr.argumentNames == nil,
      declRefExpr.moduleSelector == nil
    {
      return generateLifetimeDescriptor(
        nameToken: declRefExpr.baseName,
        lifetimeDependenceKind: lifetimeDependenceKind
      )
    }

    if let index = descriptorExpr.as(IntegerLiteralExprSyntax.self)?.representedLiteralValue {
      return .forOrdered(
        index,
        dependenceKind: lifetimeDependenceKind,
        loc: loc
      )
    }

    // Not a name, 'self', or an index, e.g. '@_lifetime(a: inout a)' where the
    // 'inout' has nothing to apply to.
    self.diagnose(
      .expected_identifier_or_index_or_self_after_lifetime_dependence(allowIndices),
      at: descriptorExpr
    )
    return nil
  }

  func generateLifetimeEntry(
    attribute node: AttributeSyntax,
    allowIndices: Bool
  ) -> BridgedLifetimeEntry? {
    self.generateWithLabeledExprListArguments(attribute: node) { args in
      guard !args.isEmpty else {
        // `@_lifetime()`. The C++ parser reports the missing specifier here.
        self.diagnose(
          .expected_identifier_or_index_or_self_after_lifetime_dependence(allowIndices),
          at: node.rightParen.map(Syntax.init) ?? Syntax(node.attributeName)
        )
        return nil
      }

      var target: BridgedLifetimeDescriptor? = nil
      var sources: [BridgedLifetimeDescriptor] = []
      var first = true
      while let arg = args.popFirst() {
        if first {
          if let targetToken = arg.label {
            target = self.generateLifetimeDescriptor(nameToken: targetToken)
          }
          first = false
        } else {
          if arg.label != nil {
            // TODO: Diagnose.
            fatalError("invalid argument label in @_lifetime attribute")
          }
        }

        if let src = self.generateLifetimeDescriptor(
          expr: arg.expression,
          allowIndices: allowIndices
        ) {
          sources.append(src)
        }
      }

      if let target {
        return .createParsed(
          self.ctx,
          range: self.generateAttrSourceRange(node),
          sources: sources.lazy.bridgedArray(in: self),
          target: target
        )
      } else {
        return .createParsed(
          self.ctx,
          range: self.generateAttrSourceRange(node),
          sources: sources.lazy.bridgedArray(in: self)
        )
      }
    }
  }

  /// E.g.
  ///   ```
  ///   @_lifetime(src1, src2)
  ///   @_lifetime(target: borrow src1, copy src2)
  ///   @_lifetime(2)
  ///   @_lifetime(self)
  ///   ```
  func generateLifetimeAttr(attribute node: AttributeSyntax) -> BridgedLifetimeAttr? {
    // A declaration attribute does not accept parameter indices; only the type
    // attribute spelling does.
    guard let entry = self.generateLifetimeEntry(attribute: node, allowIndices: false) else {
      // TODO: Diagnose?
      return nil
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      entry: entry,
      isUnderscored: node.attributeName.as(IdentifierTypeSyntax.self)?.name.text == "_lifetime"
    )
  }

  func generateMacroIntroducedDeclNameKind(declReferenceExpr node: DeclReferenceExprSyntax) -> BridgedMacroIntroducedDeclNameKind? {
    if node.argumentNames != nil {
      // TODO: Diagnose
    }
    if node.moduleSelector != nil {
      // TODO: Diagnose
    }
    guard node.argumentNames == nil && node.moduleSelector == nil else {
      return nil
    }
    switch node.baseName.rawText {
    case "arbitrary":
      return .arbitrary
    case "named":
      return .named
    case "overloaded":
      return .overloaded
    case "prefixed":
      return .prefixed
    case "suffixed":
      return .suffixed
    default:
      return nil
    }
  }

  func generateMacroIntroducedDeclName(expr node: ExprSyntax) -> BridgedMacroIntroducedDeclName? {
    let kind: BridgedMacroIntroducedDeclNameKind?
    let arguments: LabeledExprListSyntax?
    if let kindExpr = node.as(DeclReferenceExprSyntax.self) {
      kind = self.generateMacroIntroducedDeclNameKind(declReferenceExpr: kindExpr)
      arguments = nil
    } else if let callExpr =  node.as(FunctionCallExprSyntax.self) {
      if let kindExpr = callExpr.calledExpression.as(DeclReferenceExprSyntax.self) {
        kind = self.generateMacroIntroducedDeclNameKind(declReferenceExpr: kindExpr)
      } else {
        kind = nil
      }
      arguments = callExpr.arguments
    } else {
      kind = nil
      arguments = nil
    }
    guard let kind else {
      // TODO: Diagnose.
      return nil
    }

    let name: BridgedDeclNameRef
    switch kind {
    case .named, .prefixed, .suffixed:
      guard let arguments else {
        // TODO: Diagnose
        return nil
      }
      guard var arg = arguments.first?.expression else {
        // TODO: Diagnose.
        return nil
      }
      if let call = arg.as(FunctionCallExprSyntax.self), call.arguments.isEmpty {
        // E.g. 'named(foo())', use the callee to generate the name.
        arg = call.calledExpression
      }

      if let arg = arg.as(DeclReferenceExprSyntax.self) {
        name = self.generateDeclNameRef(declReferenceExpr: arg).name
      } else if arg.is(DiscardAssignmentExprSyntax.self) {
        name = BridgedDeclNameRef.createParsed(
          self.ctx,
          moduleSelector: nil,
          baseName: .init(self.ctx.getIdentifier("_"))
        )
      } else {
        // TODO: Diagnose
        fatalError("expected name")
        //return nil
      }

      if arguments.count >= 2 {
        fatalError("unexpected arguments")
        // TODO: Diagnose.
      }

    case .overloaded, .arbitrary:
      if arguments != nil {
        // TODO: Diagnose
      }
      name = BridgedDeclNameRef()
    }

    return BridgedMacroIntroducedDeclName(kind: kind, name: name)
  }

  /// E.g.
  ///   ```
  ///   @freestanding(declaration, names: named(foo())
  ///   @attached(peer, conformances: ProtocolName)
  ///   ```
  func generateMacroRoleAttr(attribute node: AttributeSyntax, attrName: SyntaxText) -> BridgedMacroRoleAttr? {
    // '@freestanding' or '@attached'.
    assert(attrName == "freestanding" || attrName == "attached")
    let syntax: BridgedMacroSyntax =
      attrName == "freestanding" ? .freestanding : .attached;
    let isAttached = syntax == .attached

    return self.generateWithLabeledExprListArguments(attribute: node) { args in
      // Macro role.
      var roleToken: TokenSyntax?
      let role = self.generateConsumingPlainIdentifierAttrOption(args: &args) {
        roleToken = $0
        return BridgedMacroRole(from: $0.rawText.bridged)
      }
      guard let role = role else {
        return nil
      }
      guard role != .none else {
        // TODO: Diagnose.
        return nil
      }
      if !role.isSupported(in: self.ctx) {
        // The C++ parser reports it but still builds the attribute, so the
        // macro does not also complain about having no roles.
        self.diagnose(
          .macro_experimental(roleToken!.text, ""),
          at: roleToken!
        )
      }
      if role.isAttached != isAttached {
        // TODO: Diagnose.
        return nil
      }

      var names: [BridgedMacroIntroducedDeclName] = []
      var conformances: [BridgedExpr] = []

      enum Argument: UInt8 {
        case names
        case conformances
        case invalid
      }
      // Assume we're in 'names:' arguments.
      var argState = AttrArgumentState<Argument, UInt8>(.names)

      LOOP: while let arg = args.popFirst() {
        // Argument state.
        if let label = arg.label {
          switch label.tokenKind {
          case .identifier("names"):
            if argState.hasSeen(.names) {
              // TODO: Diagnose duplicated 'names:'.
            }
            argState.current = .names
          case .identifier("conformances"):
            if argState.hasSeen(.conformances) {
              // TODO: Diagnose duplicated 'conformances:'.
            }
            argState.current = .conformances
          default:
            // Invalid label.
            // TODO: Diagnose `no argument with label '\(label)'`.
            argState.current = .invalid
          }
        } else if argState.current == .names && !argState.hasSeen(.names) {
          // E.g. `@attached(member, named(foo))` this is missing 'names:'
          // TODO: Diagnose to insert 'names:'
          argState.current = .names
        }

        // Argument values.
        switch argState.current {
        case .names:
          if let name = self.generateMacroIntroducedDeclName(expr: arg.expression) {
            names.append(name)
          }
        case .conformances:
          conformances.append(self.generate(expr: arg.expression))
        case .invalid:
          // Ignore the value.
          break
        }
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        syntax: syntax,
        lParenLoc: self.generateSourceLoc(node.leftParen),
        role: role,
        names: names.lazy.bridgedArray(in: self),
        conformances: conformances.lazy.bridgedArray(in: self),
        rParenLoc: self.generateSourceLoc(node.rightParen)
      )
    }
  }

  /// E.g.:
  ///   ```
  ///   @_nonSendable
  ///   @_nonSendable(_assumed)
  ///   ```
  func generateNonSendableAttr(attribute node: AttributeSyntax) -> BridgedNonSendableAttr? {
    let kind: BridgedNonSendableKind? = self.generateSingleAttrOption(
      attribute: node,
      options: ["assumed"],
      {
        switch $0.rawText {
        case "_assumed": return .assumed
        default: return nil
        }
      },
      valueIfOmitted: .specific
    )
    guard let kind else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      kind: kind
    )
  }

  // FIXME: This is a decl modifier
  func generateNonisolatedAttr(attribute node: AttributeSyntax) -> BridgedNonisolatedAttr? {
    let modifier: swift.NonIsolatedModifier? = self.generateSingleAttrOption(
      attribute: node,
      options: ["unsafe", "nonsending"],
      {
        switch $0.rawText {
        case "unsafe": return .unsafe
        case "nonsending": return .nonsending
        default: return nil
        }
      },
      valueIfOmitted: swift.NonIsolatedModifier.none
    )
    guard let modifier else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      modifier: modifier
    )
  }

  func generateInheritActorContextAttr(attribute node: AttributeSyntax) -> BridgedInheritActorContextAttr? {
    let modifier: swift.InheritActorContextModifier? = self.generateSingleAttrOption(
      attribute: node,
      options: ["always"],
      {
        switch $0.rawText {
        case "always": return .always
        default: return nil
        }
      },
      valueIfOmitted: swift.InheritActorContextModifier.none
    )
    guard let modifier else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      modifier: modifier
    )
  }

  /// E.g.:
  ///   ```
  ///   @objc
  ///   @objc(name)
  ///   @objc(nameWith:params:)
  ///   ```
  func generateObjCAttr(attribute node: AttributeSyntax) -> BridgedObjCAttr? {
    guard let arguments = node.arguments else {
      // '@objc'
      return .createParsedUnnamed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        attrNameLoc: self.generateSourceLoc(node.attributeName)
      )
    }

    guard
      let selectorPieces = arguments.as(ObjCSelectorPieceListSyntax.self),
      !selectorPieces.isEmpty
    else {
      // '@objc()' - invalid. Recover as '@objc'
      // TODO: Diagnose "error: expected name within parentheses of @objc attribute"
      return .createParsedUnnamed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        attrNameLoc: self.generateSourceLoc(node.attributeName)
      )
    }

    if selectorPieces.count == 1 && selectorPieces.first!.colon == nil {
      // '@objc(name)'
      let name = selectorPieces.first!.name
      return .createParsedNullary(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        attrNameLoc: self.generateSourceLoc(node.attributeName),
        lParenLoc: self.generateSourceLoc(node.leftParen),
        nameLoc: self.generateSourceLoc(name),
        name: self.generateIdentifier(name),
        rParenLoc: self.generateSourceLoc(node.rightParen)
      )
    } else {
      // '@objc(nameWith:params:)'
      return .createParsedSelector(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        attrNameLoc: self.generateSourceLoc(node.attributeName),
        lParenLoc: self.generateSourceLoc(node.leftParen),
        nameLocs: selectorPieces.lazy.map({ self.generateSourceLoc($0.name) }).bridgedArray(in: self),
        names: selectorPieces.lazy.map({ self.generateIdentifier($0.name) }).bridgedArray(in: self),
        rParenLoc: self.generateSourceLoc(node.rightParen)
      )
    }
  }

  /// E.g.:
  ///   ```
  ///   @implementation
  ///   @implementation(CategoryName) // error in Sema.
  ///   @_objcImplementation
  ///   @_objcImplementation(CategoryName)
  ///   ```
  func generateObjCImplementationAttr(attribute node: AttributeSyntax) -> BridgedObjCImplementationAttr? {
    let name: Identifier? = self.generateSingleAttrOption(
      attribute: node,
      self.generateIdentifier,
      valueIfOmitted: Identifier()
    )
    guard let name else {
      // Should be diagnosed by `generateSingleAttrOption`.
      return nil
    }

    let attrName = node.attributeName.as(IdentifierTypeSyntax.self)?.name.text
    let isEarlyAdopter = attrName != "implementation"

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      name: name,
      isEarlyAdopter: isEarlyAdopter
    )
  }

  /// E.g.:
  ///   ```
  ///   @_objcRuntimeName(RenamedClass)
  ///   ```
  func generateObjCRuntimeNameAttr(attribute node: AttributeSyntax) -> BridgedObjCRuntimeNameAttr? {
    let name: Identifier? = self.generateSingleAttrOption(attribute: node) {
      self.generateIdentifier($0)
    }
    guard let name else {
      // TODO: Diagnose.
      return nil
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      name: name
    )
  }

  func generateOptimizeAttr(attribute node: AttributeSyntax) -> BridgedOptimizeAttr? {
    let mode: BridgedOptimizationMode? = self.generateSingleAttrOption(
      attribute: node,
      options: ["speed", "size", "none"],
      {
        switch $0.rawText {
        case "speed": return .forSpeed
        case "size": return .forSize
        case "none": return .noOptimization
        default: return nil
        }
      }
    )
    guard let mode else {
      // TODO: Diagnose
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      mode: mode
    )
  }

  /// E.g.:
  ///   ```
  ///   @_originallyDefinedIn(module: "OriginalModule", macOS 2.0)
  ///   ```
  func generateOriginallyDefinedInAttr(attribute node: AttributeSyntax) -> [BridgedOriginallyDefinedInAttr] {
    guard
      // `@_OriginallyDefinedIn` has special argument list syntax.
      let args = node.arguments?.as(OriginallyDefinedInAttributeArgumentsSyntax.self)
    else {
      // TODO: Diagnose.
      return []
    }

    guard
      let moduleName = self.generateStringLiteralTextIfNotInterpolated(expr: args.moduleName),
      !moduleName.isEmpty
    else {
      // TODO: Diagnose
      fatalError("expected non-empty string literal without interpolations")
    }

    let atLoc = self.generateSourceLoc(node.atSign)
    let range = self.generateAttrSourceRange(node)
    let moduleNameInCtx = self.ctx.allocateCopy(string: moduleName)

    let platformVersions = self.generate(
      platformVersionList: args.platforms,
      attrName: "@_originallyDefinedIn"
    )
    if platformVersions.hadError {
      return []
    }
    if platformVersions.needsPlatformVersionDiagnostic {
      self.diagnose(
        .attr_availability_need_platform_version("@_originallyDefinedIn"),
        at: node.attributeName
      )
      return []
    }
    var result: [BridgedOriginallyDefinedInAttr] = []
    for platformVersion in platformVersions.versions {
      let attr = BridgedOriginallyDefinedInAttr.createParsed(
        ctx,
        atLoc: atLoc,
        range: range,
        moduleName: moduleNameInCtx,
        platform: platformVersion.platform,
        version: platformVersion.version
      )
      result.append(attr)
    }
    return result
  }

  func generatePrivateImportAttr(attribute node: AttributeSyntax) -> BridgedPrivateImportAttr? {
    self.generateWithLabeledExprListArguments(attribute: node) { args in
      let fileName = self.generateConsumingSimpleStringLiteralAttrOption(args: &args, label: "sourceFile")
      guard let fileName else {
        return nil
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        attrNameLoc: self.generateSourceLoc(node.attributeName),
        lParenLoc: self.generateSourceLoc(node.leftParen),
        fileName: fileName,
        rParenLoc: self.generateSourceLoc(node.rightParen)
      )
    }
  }

  /// E.g.:
  ///   ```
  ///   @_projectedValueProperty($value)
  ///   ```
  func generateProjectedValuePropertyAttr(attribute node: AttributeSyntax) -> BridgedProjectedValuePropertyAttr? {
    // `@_dynamicReplacement` has special argument list syntax
    let name = self.generateSingleAttrOption(attribute: node, { self.generateIdentifier($0) }, valueIfOmitted: Identifier())
    guard let name else {
      // TODO: Diagnose.
      return nil
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      name: name
    )
  }

  func generatePreInverseGenericsAttr(attribute node: AttributeSyntax) -> BridgedPreInverseGenericsAttr? {
    // '@_preInverseGenericsExceptCopyable' is a shorthand for
    // '@_preInverseGenerics(except: ~Copyable)'. It takes no arguments, and its
    // 'except:' type is resolved rather than written, so it cannot go through the
    // TypeRepr path below.
    if node.attributeName.as(IdentifierTypeSyntax.self)?.name.rawText
      == "_preInverseGenericsExceptCopyable"
    {
      return .createParsedExceptCopyable(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node)
      )
    }

    // Bare '@_preInverseGenerics' takes no arguments.
    guard node.arguments != nil else {
      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node)
      )
    }

    return self.generateWithLabeledExprListArguments(attribute: node) { args in
      guard let arg = args.popFirst(), arg.label?.rawText == "except" else {
        self.diagnose(.attr_pre_inverse_generics_expected_except, at: node.attributeName)
        return nil
      }
      // The 'except:' argument is a type, e.g. '~Copyable', which the parser
      // gives us in expression position.
      guard let exceptType = self.generateTypeRepr(expr: arg.expression) else {
        self.diagnose(.expected_type, at: arg.expression)
        return nil
      }
      // Whether this form requires the PreInverseGenericsExcept feature is
      // checked in Sema, by `AttributeChecker::visitPreInverseGenericsAttr`.
      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        exceptType: exceptType
      )
    }
  }

  func generateRawLayoutAttr(attribute node: AttributeSyntax) -> BridgedRawLayoutAttr? {
    self.generateWithLabeledExprListArguments(attribute: node) {
      (args: inout Slice<LabeledExprListSyntax>) -> BridgedRawLayoutAttr? in
      switch args.first?.label?.rawText {
      case "size":
        return generateSizeAlignment()
      case "like":
        return generateScalarLike()
      case "likeArrayOf":
        return generateArrayLike()
      default:
        // The C++ parser reports this at the attribute name, not at the label.
        self.diagnose(
          .attr_rawlayout_expected_label("'size', 'like', or 'likeArrayOf'"),
          at: node.attributeName
        )
        return nil
      }

      func generateSizeAlignment() -> BridgedRawLayoutAttr? {
        guard let size = generateConsumingIntegerLiteralOption(label: "size") else {
          // Should already be diagnosed.
          return nil
        }
        guard !args.isEmpty else {
          // '@_rawLayout(size: 4)' with no 'alignment:'.
          self.diagnose(
            .attr_rawlayout_expected_params("size", "alignment"),
            at: node.attributeName
          )
          return nil
        }
        guard let alignment = generateConsumingIntegerLiteralOption(label: "alignment") else {
          // Should already be diagnosed.
          return nil
        }
        return .createParsed(
          self.ctx,
          atLoc: self.generateSourceLoc(node.atSign),
          range: self.generateAttrSourceRange(node),
          size: size,
          alignment: alignment
        )
      }

      func generateScalarLike() -> BridgedRawLayoutAttr? {
        let tyR = self.generateConsumingAttrOption(args: &args, label: "like") {
          self.generateTypeRepr(expr: $0)
        }
        guard let tyR else {
          return nil
        }

        guard let moveAsLike = args.isEmpty ? false : generateConsumingMovesAsLike() else {
          return nil
        }

        return .createParsed(
          self.ctx,
          atLoc: self.generateSourceLoc(node.atSign),
          range: self.generateAttrSourceRange(node),
          like: tyR,
          moveAsLike: moveAsLike
        )
      }

      func generateArrayLike() -> BridgedRawLayoutAttr? {
        let tyR = self.generateConsumingAttrOption(args: &args, label: "likeArrayOf") {
          self.generateTypeRepr(expr: $0)
        }
        guard let tyR else {
          return nil
        }

        guard !args.isEmpty else {
          // '@_rawLayout(likeArrayOf: T)' with no 'count:'.
          self.diagnose(
            .attr_rawlayout_expected_params("likeArrayOf", "count"),
            at: node.attributeName
          )
          return nil
        }

        // 'count:' can be integer literal or a generic parameter.
        let count = self.generateConsumingAttrOption(args: &args, label: "count") {
          self.generateValueOrType(expr: $0)
        }
        guard let count else {
          return nil
        }

        guard let moveAsLike = args.isEmpty ? false : generateConsumingMovesAsLike() else {
          return nil
        }

        return .createParsed(
          self.ctx,
          atLoc: self.generateSourceLoc(node.atSign),
          range: self.generateAttrSourceRange(node),
          likeArrayOf: tyR,
          count: count,
          moveAsLike: moveAsLike
        )
      }

      func generateConsumingIntegerLiteralOption(label: SyntaxText) -> Int? {
        self.generateConsumingAttrOption(args: &args, label: label) { expr in
          // The C++ parser has a dedicated diagnostic per position here, so pick
          // the one matching the label.
          let diagnostic: CompilerDiagnostic =
            label == "alignment"
            ? .attr_rawlayout_expected_integer_alignment
            : .attr_rawlayout_expected_integer_size
          guard let integerExpr = expr.as(IntegerLiteralExprSyntax.self),
            let value = integerExpr.representedLiteralValue
          else {
            // Not an integer literal at all, or one the lexer accepted but that
            // does not fit, e.g. an overflowing literal.
            self.diagnose(diagnostic, at: expr)
            return nil
          }
          return value
        }
      }

      func generateConsumingMovesAsLike() -> Bool? {
        self.generateConsumingPlainIdentifierAttrOption(args: &args) {
          switch $0.rawText {
          case "movesAsLike":
            return true
          default:
            // TODO: Diagnose.
            fatalError("expected 'moveAsLike' in @rawLayout attribute")
          }
        }
      }
    }
  }

  // FIXME: This is a decl modifier
  func generateReferenceOwnershipAttr(attribute node: AttributeSyntax, attrName: SyntaxText)
    -> BridgedReferenceOwnershipAttr?
  {
    let kind: BridgedReferenceOwnership
    if attrName == "weak" {
      kind = .weak
      // TODO: Diagnose extraneous arguments.
    } else if attrName == "unowned" {
      kind =
        self.generateSingleAttrOption(
          attribute: node,
          {
            switch $0.rawText {
            case "unsafe": return .unmanaged
            case "safe": return .unowned
            default: return nil
            }
          },
          valueIfOmitted: .unowned
        ) ?? .unowned
    } else {
      preconditionFailure("ReferenceOwnership attribute must be 'weak' or 'unowned'")
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      kind: kind
    )
  }

  /// E.g.:
  ///   ```
  ///   @semantics("semantics_name")
  func generateSemanticsAttr(attribute node: AttributeSyntax) -> BridgedSemanticsAttr? {
    self.generateWithLabeledExprListArguments(attribute: node) { args in
      guard let value = self.generateConsumingSimpleStringLiteralAttrOption(args: &args) else {
        return nil
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        value: value
      )
    }
  }

  /// E.g.:
  ///   ```
  ///   @_target("avx2")
  ///   ```
  func generateTargetAttr(attribute node: AttributeSyntax) -> BridgedTargetAttr? {
    self.generateWithLabeledExprListArguments(attribute: node) { args in
      guard let value = self.generateConsumingSimpleStringLiteralAttrOption(args: &args) else {
        return nil
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        value: value
      )
    }
  }

  /// E.g.:
  ///   ```
  ///   @_silgen_name("external_func")
  ///   ```
  func generateSILGenNameAttr(attribute node: AttributeSyntax) -> BridgedSILGenNameAttr? {
    return self.generateWithLabeledExprListArguments(attribute: node) { args in
      guard let arg = args.popFirst() else {
        // TODO: Diagnose.
        return nil
      }

      // 'raw:'
      let isRaw: Bool
      if let label = arg.label {
        if label.rawText == "raw" {
          isRaw = true
        } else {
          // TODO: Diagnose.
          return nil
        }
      } else {
        isRaw = false
      }

      guard let name = self.generateStringLiteralTextIfNotInterpolated(expr: arg.expression) else {
        // TODO: Diagnose.
        return nil
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        name: name,
        isRaw: isRaw
      )
    }
  }

  /// E.g.:
  ///   ```
  ///   @specialized(exported: true, where T == Int)
  ///   ```
  func generateSpecializedAttr(attribute node: AttributeSyntax, attrName: SyntaxText) -> BridgedSpecializedAttr? {
    guard
      let arg = node.arguments?.as(SpecializedAttributeArgumentSyntax.self)
    else {
      // TODO: Diagnose
      return nil
    }
    let exported: Bool? = arg.exportedArgument.map {
      // The argument reuses '@_specialize's labeled argument node, but the
      // parser only builds it with the 'exported' label.
      precondition(
        $0.label.rawText == "exported",
        "(compiler bug) '@specialized' argument should only be parsed with the 'exported' label"
      )
      return $0.value.rawText == "true"
    }
    let kind: BridgedSpecializationKind? = nil
    var whereClause: BridgedTrailingWhereClause? = nil
    let targetFunction: (name: BridgedDeclNameRef, loc: BridgedDeclNameLoc)? = nil
    let spiGroups: [Identifier] = []
    let availableAttrs: [BridgedAvailableAttr] = []

    whereClause = self.generate(genericWhereClause: arg.genericWhereClause)

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      whereClause: whereClause.asNullable,
      exported: exported ?? false,
      kind: kind ?? .full,
      targetFunction: targetFunction?.name ?? BridgedDeclNameRef(),
      targetFunctionLoc: targetFunction?.loc ?? BridgedDeclNameLoc(),
      spiGroups: spiGroups.lazy.bridgedArray(in: self),
      availableAttrs: availableAttrs.lazy.bridgedArray(in: self)
    )
  }

  /// E.g.:
  ///   ```
  ///   @_specialize(exported: true, T == Int)
  ///   ```
  func generateSpecializeAttr(attribute node: AttributeSyntax, attrName: SyntaxText) -> BridgedSpecializeAttr? {
    let isPublic = attrName == "specialized"
    let rightParenOrName = Syntax(node.rightParen ?? node.attributeName.lastToken(viewMode: .sourceAccurate)!)
    guard
      var args = node.arguments?.as(SpecializeAttributeArgumentListSyntax.self)?[...]
    else {
      // `@_specialize()`: nothing, not even a 'where' clause.
      if !node.hasError {
        self.diagnose(.attr_specialize_missing_parameter_label_or_where_clause, at: rightParenOrName)
      }
      return nil
    }
    var exported: Bool?
    var kind: BridgedSpecializationKind? = nil
    var whereClause: BridgedTrailingWhereClause? = nil
    var targetFunction: (name: BridgedDeclNameRef, loc: BridgedDeclNameLoc)?
    var spiGroups: [Identifier] = []
    var availableAttrs: [BridgedAvailableAttr] = []
    // Diagnostics for the cases the parser accepts but the C++ parser rejects
    // are only needed when the parser had no complaint of its own.
    let parserDiagnosed = node.hasError

    while let arg = args.popFirst() {
      switch arg {
      case .genericWhereClause(let arg):
        whereClause = self.generate(genericWhereClause: arg)
      case .specializeTargetFunctionArgument(let arg):
        if targetFunction != nil {
          // TODO: Diangose.
        }
        targetFunction = self.generateDeclNameRef(declReferenceExpr: arg.declName)
      case .specializeAvailabilityArgument(let arg):
        availableAttrs = self.generateAvailableAttr(
          atLoc: self.generateSourceLoc(arg.availabilityLabel),
          range: self.generateSourceRange(
            start: arg.availabilityArguments.firstToken(viewMode: .all)!,
            end: arg.semicolon
          ),
          attrName: attrName,
          args: arg.availabilityArguments
        )
      case .labeledSpecializeArgument(let arg):
        // FIXME: Can be 'LabeledExprSyntax'.
        let label = String(syntaxText: arg.label.rawText)
        func diagnoseAlreadyDefined() {
          self.diagnose(.attr_specialize_parameter_already_defined(label, isPublic), at: arg.value)
        }
        switch arg.label.rawText {
        case "kind":
          if kind != nil {
            diagnoseAlreadyDefined()
          }
          switch arg.value.rawText {
          case "partial":
            kind = .partial
          case "full":
            kind = .full
          default:
            if !parserDiagnosed {
              self.diagnose(.attr_specialize_expected_partial_or_full, at: arg.value)
            }
          }

        case "exported":
          if exported != nil {
            diagnoseAlreadyDefined()
          }
          switch arg.value.rawText {
          case "true":
            exported = true
          case "false":
            exported = false
          default:
            if !parserDiagnosed {
              self.diagnose(.attr_specialize_expected_bool_value(isPublic), at: arg.value)
            }
          }

        case "spi":
          if !spiGroups.isEmpty {
            diagnoseAlreadyDefined()
          }
          guard arg.value.rawTokenKind == .identifier || arg.value.rawTokenKind == .wildcard else {
            if !parserDiagnosed {
              self.diagnose(.attr_specialize_expected_spi_name, at: arg.value)
            }
            break
          }
          spiGroups.append(self.generateIdentifier(arg.value))

        default:
          if !parserDiagnosed {
            self.diagnose(.attr_specialize_unknown_parameter_name(label, isPublic), at: arg.label)
          }
        }

        // Each parameter other than the last is followed by a comma.
        if arg.trailingComma == nil, let next = args.first, !parserDiagnosed {
          self.diagnose(
            .attr_specialize_missing_comma(isPublic),
            at: next.firstToken(viewMode: .sourceAccurate).map(Syntax.init) ?? rightParenOrName
          )
        }
      }
    }

    // The list ends with a 'where' clause.
    if whereClause == nil, !parserDiagnosed {
      self.diagnose(
        isPublic ? .attr_specialized_missing_where_clause : .attr_specialize_missing_parameter_label_or_where_clause,
        at: rightParenOrName
      )
      return nil
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      whereClause: whereClause.asNullable,
      exported: exported ?? false,
      kind: kind ?? .full,
      targetFunction: targetFunction?.name ?? BridgedDeclNameRef(),
      targetFunctionLoc: targetFunction?.loc ?? BridgedDeclNameLoc(),
      spiGroups: spiGroups.lazy.bridgedArray(in: self),
      availableAttrs: availableAttrs.lazy.bridgedArray(in: self)
    )
  }

  /// E.g.:
  ///   ```
  ///   @_spi(GroupName)
  ///   ```
  func generateSPIAccessControlAttr(attribute node: AttributeSyntax) -> BridgedSPIAccessControlAttr? {
    let hasArguments = node.arguments != nil
    let spiName: Identifier? = self.generateSingleAttrOption(attribute: node) {
      (token: TokenSyntax) -> Identifier? in
      // The name must be an identifier (or `_`); an operator, or the
      // contextual keyword `set`, is not an SPI name. Note that `nil` here must
      // be spelled `Optional.none`: `Identifier` itself expresses `nil`.
      if token.text == "set" {
        return Optional<Identifier>.none
      }
      switch token.tokenKind {
      case .identifier, .wildcard, .keyword:
        return self.generateIdentifier(token)
      default:
        return Optional<Identifier>.none
      }
    }
    guard let spiName else {
      if hasArguments {
        // The C++ parser points just past the '('.
        let loc = node.leftParen.map {
          SourceLoc(at: $0.endPositionBeforeTrailingTrivia, in: self.base)
        } ?? self.generateSourceLoc(node.attributeName)
        self.diagnose(.attr_access_expected_spi_name, at: loc)
      }
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      spiGroupName: spiName
    )
  }

  /// E.g.:
  ///   ```
  ///   @storageRestrictions(initializes: y, full, accesses: x)
  ///   ```
  func generateStorageRestrictionAttr(attribute node: AttributeSyntax) -> BridgedStorageRestrictionsAttr? {
    self.generateWithLabeledExprListArguments(attribute: node) { args -> _? in

      // @storageRestrictions(initializes: <ident>...[, accesses: <ident>...])

      // The first argument has to be labeled.
      guard let first = args.first, first.label != nil else {
        if !node.hasError {
          self.diagnose(.missing_storage_restrictions_attr_label, at: node.attributeName)
        }
        return nil
      }

      enum Argument: UInt8 {
        case initializes
        case accesses
        case invalid
      }
      var argState = AttrArgumentState<Argument, UInt8>(.invalid)
      var initializesProperties: [Identifier] = []
      var accessesProperties: [Identifier] = []

      while let arg = args.popFirst() {
        // Label.
        switch arg.label?.rawText {
        case "initializes":
          if argState.hasSeen(.initializes), let label = arg.label {
            self.diagnose(.duplicate_storage_restrictions_attr_label(self.generateIdentifier(label)), at: label)
          }
          argState.current = .initializes
        case "accesses":
          if argState.hasSeen(.accesses), let label = arg.label {
            self.diagnose(.duplicate_storage_restrictions_attr_label(self.generateIdentifier(label)), at: label)
          }
          argState.current = .accesses
        case nil:
          // An unlabeled name continues the list of the preceding label:
          // `initializes: y, full, accesses: x`.
          break
        default:
          if let label = arg.label {
            self.diagnose(.invalid_storage_restrictions_attr_label(self.generateIdentifier(label)), at: label)
          }
          argState.current = .invalid
        }

        // Value.
        func generatePropertyName(expr node: ExprSyntax) -> Identifier? {
          guard
            let node = node.as(DeclReferenceExprSyntax.self),
            node.argumentNames == nil,
            node.moduleSelector == nil
          else {
            if !arg.hasError {
              self.diagnose(.storage_restrictions_attr_expected_name, at: node)
            }
            return nil
          }
          return self.generateIdentifier(node.baseName)
        }

        switch argState.current {
        case .initializes:
          if let name = generatePropertyName(expr: arg.expression) {
            initializesProperties.append(name)
          }
        case .accesses:
          if let name = generatePropertyName(expr: arg.expression) {
            accessesProperties.append(name)
          }
        case .invalid:
          // Ignore the value.
          break
        }
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        initializes: initializesProperties.lazy.bridgedArray(in: self),
        accesses: accessesProperties.lazy.bridgedArray(in: self)
      )
    }
  }

  /// E.g.:
  ///   ```
  ///   @_swift_native_objc_runtime_base(FooBase)
  ///   ```
  func generateSwiftNativeObjCRuntimeBaseAttr(attribute node: AttributeSyntax) -> BridgedSwiftNativeObjCRuntimeBaseAttr? {
    let name: Identifier? = self.generateSingleAttrOption(attribute: node) {
      self.generateIdentifier($0)
    }
    guard let name else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      name: name
    )
  }
  
  /// E.g.:
  ///   ```
  ///   @diagnose(DiagGroupID, as: Behavior, reason: String?)
  ///   ```
  func generateDiagnoseAttr(attribute node: AttributeSyntax) -> BridgedDiagnoseAttr? {
    return self.generateWithLabeledExprListArguments(attribute: node) { args in
      var groupText = ""
      guard let diagGroupIdentifier: swift.Identifier =
          self.generateConsumingAttrOption(args: &args, label: nil, { expr in
            guard let declRefExpr = expr.as(DeclReferenceExprSyntax.self) else {
              return nil
            }
            groupText = declRefExpr.baseName.text
            return self.generateIdentifier(declRefExpr.baseName)
          })
      else {
        return nil
      }

      // An unknown group is diagnosed, and the attribute dropped once the rest
      // of its arguments have been checked, as in the C++ parser.
      let groupIsKnown = BridgedDiagnoseAttr_isKnownGroup(diagGroupIdentifier)
      if !groupIsKnown {
        self.diagnose(
          .attr_diagnose_unknown_diagnostic_group_identifier(groupText),
          at: node.attributeName
        )
      }

      guard let behavior: swift.WarningGroupBehavior =
          self.generateConsumingAttrOption(args: &args, label: "as", { expr in
            guard let declRefExpr = expr.as(DeclReferenceExprSyntax.self) else {
              return nil
            }
            switch declRefExpr.baseName.text {
            case "error": return swift.WarningGroupBehavior.error
            case "warning": return swift.WarningGroupBehavior.warning
            case "ignored": return swift.WarningGroupBehavior.ignored
            default:
              self.diagnose(
                .attr_diagnose_expected_known_behavior(declRefExpr.baseName.text), at: node.attributeName)
              return nil
            }
          })
      else {
        return nil
      }

      let reason: BridgedStringRef
      if let userSpecifiedReason = self.generateConsumingSimpleStringLiteralAttrOption(
        args: &args, label: "reason")
      {
        reason = userSpecifiedReason
      } else {
        reason = allocateBridgedString("")
      }

      guard groupIsKnown else {
        return nil
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        diagGroupName: diagGroupIdentifier,
        behavior: behavior,
        reason: reason
      )
    }
  }

  /// E.g.:
  ///   ```
  ///   @transpose(of: foo(_:), wrt: self)
  ///   ```
  func generateTransposeAttr(attribute node: AttributeSyntax) -> BridgedTransposeAttr? {
    guard let args = node.arguments?.as(DerivativeAttributeArgumentsSyntax.self) else {
      fatalError("(compiler bug) invalid arguments for @derivative attribute")
    }
    guard let originalDecl = self.generateDerivativeOriginalDecl(expr: args.originalDeclName) else {
      return nil
    }

    if args.accessorSpecifier != nil {
      // Only '@derivative' has an accessor form; '@transpose' has no grammar for
      // one, so the C++ parser stops at the offending token. `generateDerivativeOriginalDecl()`
      // above has already reported it, so just bail out.
      return nil
    }

    let parameters = self.generate(differentiabilityWithRespectToArgument: args.arguments)

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      baseType: originalDecl.baseType.asNullable,
      originalName: originalDecl.declName,
      originalNameLoc: originalDecl.declNameLoc,
      params: parameters
    )
  }

  /// E.g.:
  ///   ```
  ///   @_typeEraser(MyProtocol)
  ///   ```
  func generateTypeEraserAttr(attribute node: AttributeSyntax) -> BridgedTypeEraserAttr? {
    // FIXME: Should be normal LabeledExprListSyntax arguments.
    // FIXME: Error handling
    let type: BridgedTypeRepr? = self.generateSingleAttrOption(attribute: node, { token in
      // FIXME: Module selector?
      let nameRef = self.generateDeclNameRef(moduleSelector: nil, baseName: token)
      return BridgedUnqualifiedIdentTypeRepr.createParsed(
        self.ctx,
        name: nameRef.name,
        loc: nameRef.loc
      ).asTypeRepr
    })
    guard let type else {
      return nil
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      typeExpr: .createParsed(self.ctx, type: type)
    )
  }

  /// E.g.:
  ///   ```
  ///   @_unavailableFromAsync
  ///   @_unavailableFromAsync(message: "use fooBar(_:) instead")
  ///   ```
  func generateUnavailableFromAsyncAttr(attribute node: AttributeSyntax) -> BridgedUnavailableFromAsyncAttr? {
    let attrName = "_unavailableFromAsync"
    var message: BridgedStringRef? = nil
    if let arguments = node.arguments {
      // Malformed syntax has already been diagnosed by the parser.
      if arguments.hasError || node.rightParen == nil || node.rightParen?.presence == .missing
        || node.unexpectedBetweenArgumentsAndRightParen != nil
      {
        return nil
      }
      guard let args = arguments.as(LabeledExprListSyntax.self) else {
        return nil
      }
      guard let arg = args.first else {
        // `@_unavailableFromAsync()`: the message is missing.
        if let rightParen = node.rightParen {
          self.diagnose(
            .attr_expected_string_literal(attrName),
            at: rightParen,
            fixIts: [
              .init(
                replace: self.generateCharSourceRange(
                  start: rightParen.positionAfterSkippingLeadingTrivia,
                  length: SourceLength(utf8Length: 0)
                ),
                with: "\"<#error message#>\""
              )
            ]
          )
        }
        return nil
      }

      // `message="..."`: the user wrote '=' where ':' belongs.
      if arg.label == nil, let sequence = arg.expression.as(SequenceExprSyntax.self),
        sequence.elements.count == 3,
        sequence.elements.first?.as(DeclReferenceExprSyntax.self)?.baseName.text == "message",
        let assignment = sequence.elements.dropFirst().first?.as(AssignmentExprSyntax.self)
      {
        self.diagnose(
          .replace_equal_with_colon_for_value,
          at: assignment,
          fixIts: [.init(replace: self.generateCharSourceRange(assignment), with: ": ")]
        )
        return nil
      }

      if let label = arg.label {
        if label.rawText != "message" {
          if label.rawText == "_" {
            // The user wrote an explicitly omitted label (`_:`).
            self.diagnose(
              .attr_expected_label("message", attrName),
              at: label,
              fixIts: [.init(replace: self.generateCharSourceRange(label), with: "message")]
            )
          } else {
            self.diagnose(
              .attr_unknown_option(String(syntaxText: label.rawText), attrName),
              at: label,
              fixIts: [.init(replace: self.generateCharSourceRange(label), with: "message")]
            )
            return nil
          }
        }
      } else {
        self.diagnose(
          .attr_expected_label("message", attrName),
          at: arg.expression,
          fixIts: [
            .init(
              replace: self.generateCharSourceRange(
                start: arg.expression.positionAfterSkippingLeadingTrivia,
                length: SourceLength(utf8Length: 0)
              ),
              with: "message: "
            )
          ]
        )
      }

      let isStringLiteral = arg.expression.is(StringLiteralExprSyntax.self) || arg.expression.is(SimpleStringLiteralExprSyntax.self)
      guard isStringLiteral else {
        self.diagnose(
          .attr_expected_string_literal(attrName),
          at: arg.expression,
          fixIts: [.init(replace: self.generateCharSourceRange(arg.expression), with: "\"<#error message#>\"")]
        )
        return nil
      }
      message = self.generateStringLiteralTextIfNotInterpolated(expr: arg.expression)
      guard message != nil else {
        return nil
      }
      if let extra = args.dropFirst().first {
        self.diagnose(.extraneousArgumentsInAttribute(node, extra))
      }
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      message: self.ctx.allocateCopy(string: message ?? "")
    )
  }

  /// E.g.
  ///   ```
  ///   @unsafe
  ///   @unsafe(always)
  ///   ```
  func generateUnsafeAttr(attribute node: AttributeSyntax) -> BridgedUnsafeAttr? {
    let isAlways: Bool? = self.generateSingleAttrOption(
      attribute: node,
      options: ["always"],
      {
        switch $0.rawText {
        case "always": return true
        default: return nil
        }
      },
      valueIfOmitted: false
    )
    guard let isAlways else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      isAlways: isAlways
    )
  }

  func generateUnavailableInEmbeddedAttr(attribute node: AttributeSyntax) -> BridgedAvailableAttr? {
    if ctx.langOpts.hasFeature(.Embedded) {
      return BridgedAvailableAttr.createUnavailableInEmbedded(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node)
      )
    } else {
      // For non-Embedded mode, ignore it.
      return nil
    }
  }

  func generateSimpleDeclAttr(attribute node: AttributeSyntax, kind: swift.DeclAttrKind) -> BridgedDeclAttribute? {
    // TODO: Diagnose extraneous arguments.
    // TODO: Diagnose if `kind` is a modifier.
    return BridgedDeclAttribute.createSimple(
      self.ctx,
      kind: kind,
      atLoc: self.generateSourceLoc(node.atSign),
      nameLoc: self.generateSourceLoc(node.attributeName)
    )
  }

  func generateCustomAttr(attribute node: AttributeSyntax) -> BridgedCustomAttr? {
    let type = self.generate(type: node.attributeName)

    let argList: BridgedArgumentList?
    let initContext: BridgedCustomAttributeInitializer?
    if let args = node.arguments {
      guard let args = args.as(LabeledExprListSyntax.self) else {
        // TODO: Diagnose?
        return nil
      }

      if !self.declContext.isLocalContext {
        initContext = BridgedCustomAttributeInitializer.create(declContext: self.declContext)
      } else {
        initContext = nil
      }
      argList = withDeclContext(initContext?.asDeclContext ?? self.declContext) {
        self.generateArgumentList(
          leftParen: node.leftParen,
          labeledExprList: args,
          rightParen: node.rightParen,
          trailingClosure: nil,
          additionalTrailingClosures: nil
        )
      }
    } else {
      argList = nil
      initContext = nil
    }

    return .createParsed(
      atLoc: self.generateSourceLoc(node.atSign),
      type: type,
      declContext: declContext,
      initContext: initContext.asNullable,
      argumentList: argList.asNullable
    )
  }

  func generateAttrSourceRange(_ node: AttributeSyntax) -> SourceRange {
    guard let firstNameTok = node.attributeName.firstToken(viewMode: .sourceAccurate) else {
      return .init()
    }
    return self.generateSourceRange(start: firstNameTok, end: node.lastToken(viewMode: .sourceAccurate)!)
  }

  /// The source text between a string literal's quotes, exactly as written.
  ///
  /// The C++ parser stores attribute strings this way and cooks them when they
  /// are printed or used, so a multi-line literal keeps its first newline and
  /// the indentation before the closing quotes, and an escape stays an escape.
  private func rawStringLiteralContent(open: TokenSyntax, close: TokenSyntax) -> BridgedStringRef {
    let start = open.endPositionBeforeTrailingTrivia.utf8Offset
    let end = close.positionAfterSkippingLeadingTrivia.utf8Offset
    return SyntaxText(
      baseAddress: self.base.baseAddress! + start,
      count: max(end - start, 0)
    ).bridged
  }

  func generateStringLiteralTextIfNotInterpolated(expr node: some ExprSyntaxProtocol) -> BridgedStringRef? {
    if let literal = node.as(SimpleStringLiteralExprSyntax.self) {
      return rawStringLiteralContent(open: literal.openingQuote, close: literal.closingQuote)
    } else if let literal = node.as(StringLiteralExprSyntax.self) {
      guard literal.segments.allSatisfy({ $0.is(StringSegmentSyntax.self) }) else {
        self.diagnose(.forbiddenInterpolatedStringArgument(node))
        return nil
      }
      return rawStringLiteralContent(open: literal.openingQuote, close: literal.closingQuote)
    }
    self.diagnose(.expectedStringLiteralArgument(node))
    return nil
  }

  /// Convenient method for processing an attribute with `LabeledExprListSyntax`.
  ///
  /// * Diagnose if the arguments are missing.
  /// * Call `generatorFunction` callback function with a mutable _slice_ of the
  ///   labeled expression list. The callback should "consume" the processed
  ///   arguments and return the result.
  /// * If the arguments is not empty after the callback, it's diagnosed.
  func generateWithLabeledExprListArguments<T>(
    attribute node: AttributeSyntax,
    _ generatorFunction: (inout Slice<LabeledExprListSyntax>) throws -> T?
  ) rethrows -> T? {
    guard var args = node.arguments?.as(LabeledExprListSyntax.self)?[...] else {
      self.diagnoseExpectedAttributeArguments(node)
      return nil
    }
    guard let result = try generatorFunction(&args) else {
      return nil
    }
    if let extra = args.popFirst() {
      self.diagnose(.extraneousArgumentsInAttribute(node, extra))
    }
    return result
  }

  func generateConsumingAttrOption<R>(
    args: inout Slice<LabeledExprListSyntax>,
    label: SyntaxText?,
    _ valueGeneratorFunction: (ExprSyntax) -> R?
  ) -> R? {
    guard let arg = args.first else {
      // TODO: Diagnose.
      return nil
    }
    guard arg.label?.rawText == label else {
      // TODO: Diagnose.
      return nil
    }
    // Label matched. Consume the argument even if the value is not valid.
    args.removeFirst()

    return valueGeneratorFunction(arg.expression)
  }

  func generateConsumingPlainIdentifierAttrOption<R>(
    args: inout Slice<LabeledExprListSyntax>,
    _ valueGeneratorFunction: (TokenSyntax) -> R?
  ) -> R? {
    return generateConsumingAttrOption(args: &args, label: nil) {
      if let declRefExpr = $0.as(DeclReferenceExprSyntax.self), declRefExpr.argumentNames == nil, declRefExpr.moduleSelector == nil {
        return valueGeneratorFunction(declRefExpr.baseName)
      } else if let discardExpr = $0.as(DiscardAssignmentExprSyntax.self) {
        return valueGeneratorFunction(discardExpr.wildcard)
      }
      // TODO: Diagnose.
      return nil
    }
  }

  func generateConsumingSimpleStringLiteralAttrOption(
    args: inout Slice<LabeledExprListSyntax>,
    label: SyntaxText? = nil
  ) -> BridgedStringRef? {
    return self.generateConsumingAttrOption(args: &args, label: label) {
      self.generateStringLiteralTextIfNotInterpolated(expr: $0)
    }
  }

  /// Extracts single identifier token argument from the `attribute`, calls the
  /// specified function with the token, then returns the result. If `valueIfOmitted`
  /// is not nil, and there's no argument clause in the `attribute`, this returns
  /// the `valueIfOmitted` value.
  /// Returns `nil` if the argument was not a single token, the callback returned `nil`,
  /// or there was no argument clause and `valueIfOmitted` is not specified. In such cases,
  /// this emits diagnostics.
  func generateSingleAttrOption<Result>(
    attribute node: AttributeSyntax,
    options: [String] = [],
    _ valueGeneratorFunction: (TokenSyntax) -> Result?,
    valueIfOmitted: Result? = nil
  ) -> Result? {
    guard node.arguments != nil else {
      if let valueIfOmitted {
        return valueIfOmitted
      }
      self.diagnoseExpectedAttributeArguments(node)
      return nil
    }

    let attrName = node.attributeName.trimmedDescription

    return self.generateWithLabeledExprListArguments(attribute: node) { args in
      // The identifier that was written, if the argument was one.
      var written: TokenSyntax? = nil
      let result = self.generateConsumingPlainIdentifierAttrOption(args: &args) { token in
        written = token
        return valueGeneratorFunction(token)
      }

      // Diagnose as the C++ parser's `parseSingleAttrOption` does, which
      // points at the attribute name. Only attributes that list their options
      // are diagnosed.
      if !options.isEmpty {
        if result == nil {
          if let written {
            self.diagnose(.attr_unknown_option(written.text, attrName), at: node.attributeName)
          } else {
            self.diagnose(.attr_expected_option_such_as(attrName, options[0]), at: node.attributeName)
          }
        } else if !args.isEmpty {
          self.diagnose(.attr_expected_rparen(attrName, false), at: node.attributeName)
          return nil
        }
      }
      return result
    }
  }
}

// MARK: - Decl modifiers
extension ASTGenVisitor {
  func generate(declModifier node: DeclModifierSyntax) -> BridgedDeclAttribute? {
    switch node.name.keywordKind {
    case .private:
      return self.generateAccessControlAttr(declModifier: node, level: .private)
    case .fileprivate:
      return self.generateAccessControlAttr(declModifier: node, level: .fileprivate)
    case .internal:
      return self.generateAccessControlAttr(declModifier: node, level: .internal)
    case .package:
      return self.generateAccessControlAttr(declModifier: node, level: .package)
    case .public:
      return self.generateAccessControlAttr(declModifier: node, level: .public)
    case .open:
      return self.generateAccessControlAttr(declModifier: node, level: .open)
    case .nonisolated:
      return self.generateNonisolatedAttr(declModifier: node)?.asDeclAttribute
    case .weak, .unowned:
      return self.generateReferenceOwnershipAttr(declModifier: node)?.asDeclAttribute
    default:
      // Other modifiers are all "simple" attributes.
      let kind = BridgedOptionalDeclAttrKind(from: node.name.rawText.bridged)
      guard kind.hasValue, BridgedDeclAttribute.isDeclModifier(kind.value) else {
        // Not a modifier at all. The parser only produces a DeclModifierSyntax
        // for a known modifier keyword, so reaching here means recovery
        // produced something else and has already diagnosed it. Dropping the
        // modifier is enough; inventing a diagnostic here would duplicate it.
        return nil
      }
      return self.generateSimpleDeclAttr(declModifier: node, kind: kind.value)
    }
  }

  func generateAccessControlAttr(declModifier node: DeclModifierSyntax, level: swift.AccessLevel)
    -> BridgedDeclAttribute?
  {
    if let detail = node.detail {
      guard detail.detail.rawText == "set" else {
        // The only argument the grammar allows here is '(set)'; anything else is
        // recovery output that the parser has already diagnosed. Treat the
        // modifier as if it had no argument.
        return BridgedAccessControlAttr.createParsed(
          self.ctx,
          range: self.generateSourceRange(node),
          accessLevel: level
        ).asDeclAttribute
      }
      return BridgedSetterAccessAttr.createParsed(
        self.ctx,
        range: self.generateSourceRange(node),
        accessLevel: level
      ).asDeclAttribute
    } else {
      return BridgedAccessControlAttr.createParsed(
        self.ctx,
        range: self.generateSourceRange(node),
        accessLevel: level
      ).asDeclAttribute
    }
  }

  func generateNonisolatedAttr(declModifier node: DeclModifierSyntax) -> BridgedNonisolatedAttr? {
    let modifier: swift.NonIsolatedModifier
    switch node.detail?.detail.rawText {
    case "unsafe":
      modifier = .unsafe
    case "nonsending":
      modifier = .nonsending
    case nil:
      modifier = .none
    default:
      // The grammar only admits 'unsafe' and 'nonsending'; anything else is
      // recovery output that the parser has already diagnosed. Recover as
      // plain 'nonisolated'.
      modifier = .none
    }

    return BridgedNonisolatedAttr.createParsed(
      self.ctx,
      atLoc: nil,
      range: self.generateSourceRange(node),
      modifier: modifier
    )
  }

  func generateReferenceOwnershipAttr(declModifier node: DeclModifierSyntax) -> BridgedReferenceOwnershipAttr? {
    // 'weak' -> .weak
    // 'weak(<unexpected>)' -> .weak (with diagnostics)
    // 'unowned' -> .unowend
    // 'unowned(safe)' -> .unowned
    // 'unowend(unsafe)' -> .unmanaged
    // 'unowend(<unexpected>)' -> .unowned (with diagnostics)
    let kind: BridgedReferenceOwnership
    switch node.name.keywordKind {
    case .weak:
      kind = .weak
      // 'weak' takes no argument; an argument is recovery output that the parser
      // has already diagnosed, so ignore it.
    case .unowned:
      switch node.detail?.detail.rawText {
      case "safe", nil:
        kind = .unowned
      case "unsafe":
        kind = .unmanaged
      case let text?:
        // Unlike most malformed modifier arguments, the parser accepts an
        // arbitrary identifier here without complaint, so ASTGen has to be the
        // one to reject it -- otherwise this would be accepted where the C++
        // parser rejects it.
        self.diagnose(
          .attr_unknown_option(String(syntaxText: text), "unowned"),
          at: node.name
        )
        kind = .unowned
      }
    default:
      preconditionFailure("ReferenceOwnership modifier must be 'weak' or 'unowned'")
    }

    return .createParsed(
      self.ctx,
      atLoc: nil,
      range: self.generateSourceRange(node),
      kind: kind
    )
  }

  /// E.g.:
  ///   ```
  ///   @_scopeParams(a, b)
  ///   ```
  func generateScopeParamsAttr(attribute node: AttributeSyntax) -> BridgedScopeParamsAttr? {
    guard let params = self.generateScopeParams(attribute: node) else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      params: params.lazy.bridgedArray(in: self)
    )
  }

  func generateCallableAttr(attribute node: AttributeSyntax) -> BridgedCallableAttr? {
    guard self.generateCallableAttrArgument(attribute: node) else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node)
    )
  }

  /// Checks that the argument of a `@callable` attribute in either form is
  /// `once`, which is the only argument. Diagnoses and returns `false`
  /// otherwise.
  func generateCallableAttrArgument(attribute node: AttributeSyntax) -> Bool {
    guard node.arguments != nil else {
      self.diagnose(.expectedLParenInCallableAttribute(node))
      return false
    }
    let isOnce: Bool? = self.generateWithLabeledExprListArguments(attribute: node) { args in
      guard let arg = args.first else {
        self.diagnose(.expectedOnceInCallableAttribute(node))
        return nil
      }
      guard arg.label == nil,
        let declRef = arg.expression.as(DeclReferenceExprSyntax.self),
        declRef.argumentNames == nil, declRef.moduleSelector == nil,
        declRef.baseName.rawText == "once"
      else {
        self.diagnose(.expectedOnceInCallableAttribute(arg))
        return nil
      }
      args = args.dropFirst()
      return true
    }
    return isOnce ?? false
  }



  func generateSimpleDeclAttr(declModifier node: DeclModifierSyntax, kind: swift.DeclAttrKind) -> BridgedDeclAttribute? {
    // TODO: Diagnose non-modifier kind? Parser shouldn't accept them, though.
    // Simple modifier don't accept any detail.
    precondition(node.detail == nil, "decl modifiers can't have arguments except access control modifiers")
    return BridgedDeclAttribute.createSimple(
      self.ctx,
      kind: kind,
      atLoc: nil,
      nameLoc: self.generateSourceLoc(node.name)
    )
  }
}

extension ASTGenVisitor {
  func generate(generatedAttributeClauseFile node: AttributeClauseFileSyntax) -> BridgedDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)

    // Attach the attribute list to a implicit 'MissingDecl' as the placeholder.
    let decl = BridgedMissingDecl.create(
      self.ctx,
      declContext: self.declContext,
      loc: self.generateSourceLoc(node.endOfFileToken)
    ).asDecl
    decl.attachParsedAttrs(attrs.attributes)
    return decl
  }
}

/// Simpler helper for handling attribute arguments in "generate" functions.
struct AttrArgumentState<Flag: RawRepresentable, SeenStorage: FixedWidthInteger> where Flag.RawValue: FixedWidthInteger {
  private var seen: SeenStorage = 0

  var current: Flag {
    didSet {
      let flagBitIndex = current.rawValue
      precondition(flagBitIndex < SeenStorage.bitWidth)
      seen |= 1 << flagBitIndex
    }
  }

  init(_ initial: Flag) {
    self.current = initial
  }

  func hasSeen(_ flag: Flag) -> Bool {
    return (seen & (1 << flag.rawValue)) != 0
  }
}
