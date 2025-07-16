//===--- DeclAttrs.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2023 Apple Inc. and the Swift project authors
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
    var staticLoc: BridgedSourceLoc
  }

  func generateDeclAttributes(_ node: some WithAttributesSyntax & WithModifiersSyntax, allowStatic: Bool) -> DeclAttributesResult {
    var attrs = BridgedDeclAttributes()
    var staticSpelling: BridgedStaticSpelling = .none
    var staticLoc: BridgedSourceLoc = nil

    // Comments.
    COMMENT: if
      self.ctx.langOptsAttachCommentsToDecls,
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
      // FIXME: Diagnostics for duplicated attrs.
      attrs.add(attr)
    }

    // '@' attributes.
    self.generateDeclAttributes(attributeList: node.attributes, handler: addAttribute(_:))

    func genStatic(node: DeclModifierSyntax, spelling: BridgedStaticSpelling) {
      // TODO: Diagnose duplicated attrs.
      // TODO: Diagnose if allowStatic is false.
      // TODO: Diagnose 'class' for non-class decls.
      // TODO: Diagnose if not current decl context is not a type context.
      // FIXME: Model static/class as a DeclAttribute.
      staticLoc = self.generateSourceLoc(node)
      staticSpelling = spelling
    }

    for node in node.modifiers {
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
      let attrKind = BridgedDeclAttrKind(from: attrName.bridged)
      switch attrKind {
      case .ABI:
        return handle(self.generateABIAttr(attribute: node)?.asDeclAttribute)
      case .alignment:
        return handle(self.generateAlignmentAttr(attribute: node)?.asDeclAttribute)
      case .allowFeatureSuppression:
        return handle(self.generateAllowFeatureSuppressionAttr(attribute: node, attrName: attrName)?.asDeclAttribute)
      case .available:
        return self.generateAvailableAttr(attribute: node, attrName: attrName).forEach { handle($0.asDeclAttribute) }
      case .backDeployed:
        return self.generateBackDeployedAttr(attribute: node).forEach { handle($0.asDeclAttribute) }
      case .cDecl:
        return handle(self.generateCDeclAttr(attribute: node)?.asDeclAttribute)
      case .derivative:
        return handle(self.generateDerivativeAttr(attribute: node)?.asDeclAttribute)
      case .differentiable:
        return handle(self.generateDifferentiableAttr(attribute: node)?.asDeclAttribute)
      case .dynamicReplacement:
        return handle(self.generateDynamicReplacementAttr(attribute: node)?.asDeclAttribute)
      case .documentation:
        return handle(self.generateDocumentationAttr(attribute: node)?.asDeclAttribute)
      case .effects:
        return handle(self.generateEffectsAttr(attribute: node)?.asDeclAttribute)
      case .exclusivity:
        return handle(self.generateExclusivityAttr(attribute: node)?.asDeclAttribute)
      case .expose:
        return handle(self.generateExposeAttr(attribute: node)?.asDeclAttribute)
      case .extern:
        return handle(self.generateExternAttr(attribute: node)?.asDeclAttribute)
      case .implements:
        return handle(self.generateImplementsAttr(attribute: node)?.asDeclAttribute)
      case .inline:
        return handle(self.generateInlineAttr(attribute: node)?.asDeclAttribute)
      case .lifetime:
        return handle(self.generateLifetimeAttr(attribute: node)?.asDeclAttribute)
      case .macroRole:
        return handle(self.generateMacroRoleAttr(attribute: node, attrName: attrName)?.asDeclAttribute)
      case .nonSendable:
        return handle(self.generateNonSendableAttr(attribute: node)?.asDeclAttribute)
      case .objC:
        return handle(self.generateObjCAttr(attribute: node)?.asDeclAttribute)
      case .objCImplementation:
        return handle(self.generateObjCImplementationAttr(attribute: node)?.asDeclAttribute)
      case .objCRuntimeName:
        return handle(self.generateObjCRuntimeNameAttr(attribute: node)?.asDeclAttribute)
      case .optimize:
        return handle(self.generateOptimizeAttr(attribute: node)?.asDeclAttribute)
      case .originallyDefinedIn:
        return self.generateOriginallyDefinedInAttr(attribute: node).forEach { handle($0.asDeclAttribute) }
      case .privateImport:
        return handle(self.generatePrivateImportAttr(attribute: node)?.asDeclAttribute)
      case .projectedValueProperty:
        return handle(self.generateProjectedValuePropertyAttr(attribute: node)?.asDeclAttribute)
      case .rawLayout:
        return handle(self.generateRawLayoutAttr(attribute: node)?.asDeclAttribute)
      case .section:
        return handle(self.generateSectionAttr(attribute: node)?.asDeclAttribute)
      case .semantics:
        return handle(self.generateSemanticsAttr(attribute: node)?.asDeclAttribute)
      case .silGenName:
        return handle(self.generateSILGenNameAttr(attribute: node)?.asDeclAttribute)
      case .specialize:
        return handle(self.generateSpecializeAttr(attribute: node, attrName: attrName)?.asDeclAttribute)
      case .spiAccessControl:
        return handle(self.generateSPIAccessControlAttr(attribute: node)?.asDeclAttribute)
      case .storageRestrictions:
        return handle(self.generateStorageRestrictionAttr(attribute: node)?.asDeclAttribute)
      case .swiftNativeObjCRuntimeBase:
        return handle(self.generateSwiftNativeObjCRuntimeBaseAttr(attribute: node)?.asDeclAttribute)
      case .transpose:
        return handle(self.generateTransposeAttr(attribute: node)?.asDeclAttribute)
      case .typeEraser:
        return handle(self.generateTypeEraserAttr(attribute: node)?.asDeclAttribute)
      case .unavailableFromAsync:
        return handle(self.generateUnavailableFromAsyncAttr(attribute: node)?.asDeclAttribute)
      case .reasync:
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .atReasync))
      case .rethrows:
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .atRethrows))
      case .concurrent:
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .concurrent))
      case .none where attrName == "_unavailableInEmbedded":
        return handle(self.generateUnavailableInEmbeddedAttr(attribute: node)?.asDeclAttribute)

      // Renamed attributes.
      case .none where attrName == "_functionBuilder":
        // TODO: Diagnostics. '_functionBuilder' is renamed to 'resultBuilder'
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .resultBuilder))
      case .none where attrName == "_inlineable":
        // TODO: Diagnose.
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .inlinable))
      case .none where attrName == "inlineable":
        // TODO: Diagnose.
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .inlinable))
      case .none where attrName == "_versioned":
        // TODO: Diagnose.
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: .usableFromInline))

      // Simple attributes.
      case .addressableSelf,
        .addressableForDependencies,
        .alwaysEmitConformanceMetadata,
        .alwaysEmitIntoClient,
        .atReasync,
        .atRethrows,
        .borrowed,
        .compilerInitialized,
        .constVal,
        .constInitialized,
        .dynamicCallable,
        .eagerMove,
        .exported,
        .discardableResult,
        .disfavoredOverload,
        .dynamicMemberLookup,
        .emitAssemblyVisionRemarks,
        .extractConstantsFromMembers,
        .fixedLayout,
        .frozen,
        .gkInspectable,
        .globalActor,
        .hasInitialValue,
        .hasMissingDesignatedInitializers,
        .hasStorage,
        .ibAction,
        .ibDesignable,
        .ibInspectable,
        .ibOutlet,
        .ibSegueAction,
        .implementationOnly,
        .implicitSelfCapture,
        .inheritsConvenienceInitializers,
        .inlinable,
        .isolated,
        .lexicalLifetimes,
        .lldbDebuggerFunction,
        .mainType,
        .marker,
        .moveOnly,
        .noAllocation,
        .noDerivative,
        .noEagerMove,
        .noExistentials,
        .noRuntime,
        .noImplicitCopy,
        .noLocks,
        .noMetadata,
        .noObjCBridging,
        .nonEphemeral,
        .nonEscapable,
        .nonObjC,
        .nonOverride,
        .nsApplicationMain,
        .nsCopying,
        .nsManaged,
        .objCMembers,
        .objCNonLazyRealization,
        .preconcurrency,
        .preInverseGenerics,
        .propertyWrapper,
        .requiresStoredPropertyInits,
        .resultBuilder,
        .safe,
        .sendable,
        .sensitive,
        .spiOnly,
        .showInInterface,
        .specializeExtension,
        .staticExclusiveOnly,
        .testable,
        .transparent,
        .uiApplicationMain,
        .unsafe,
        .unsafeInheritExecutor,
        .unsafeNoObjCTaggedPointer,
        .unsafeNonEscapableResult,
        .usableFromInline,
        .used,
        .warnUnqualifiedAccess,
        .weakLinked:

        return handle(self.generateSimpleDeclAttr(attribute: node, kind: attrKind))

      // Modifers.
      case .accessControl:
        // TODO: Diagnose and generateAccessControl().
        fatalError("unimplemented (access control modifier parsed as attributes)")
      case .nonisolated:
        // TODO: Diagnose.
        return handle(self.generateNonisolatedAttr(attribute: node)?.asDeclAttribute)
      case .referenceOwnership:
        // TODO: Diagnose.
        return handle(self.generateReferenceOwnershipAttr(attribute: node, attrName: attrName)?.asDeclAttribute)
      case .inheritActorContext:
        return handle(self.generateInheritActorContextAttr(attribute: node)?.asDeclAttribute)

      case .async,
        .consuming,
        .borrowing,
        .actor,
        .distributedActor,
        .required,
        .optional,
        .lazy,
        .dynamic,
        .infix,
        .prefix,
        .postfix,
        .legacyConsuming,
        .mutating,
        .nonMutating,
        .convenience,
        .override,
        .indirect,
        .final,
        .knownToBeLocal,
        .compileTimeLiteral:

        // generateSimpleDeclAttr will diagnose and fix-it to change it to modifiers.
        return handle(self.generateSimpleDeclAttr(attribute: node, kind: attrKind))

      // 'RejectByParser', these attribute kind should not be parsed as built-in attributes.
      case .rawDocComment,
        .objCBridged,
        .synthesizedProtocol,
        .staticInitializeObjCMetadata,
        .restatedObjCConformance,
        .clangImporterSynthesizedType,
        .forbidSerializingReference,
        .custom,
        .setterAccess:
        assert(BridgedDeclAttribute.shouldBeRejectedByParser(attrKind))
        // Fall back to CustomAttr.
        break

      case .none:
        // Fall back to CustomAttr.
        break
      }
    }

    return handle(self.generateCustomAttr(attribute: node)?.asDeclAttribute)
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
      abiDecl = self.generate(variableDecl: varDecl)
    case .missing(_):
      // This error condition will have been diagnosed in SwiftSyntax.
      abiDecl = nil
    }

    // TODO: Diagnose if `abiDecl` has a body/initial value/etc.
    // The C++ parser considers it syntactically invalid but SwiftSyntax does not.

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
      guard let value, value > 0 else {
        // TODO: Diagnose.
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

    let features = args.compactMap(in: self) { arg -> BridgedIdentifier? in
      guard arg.label == nil,
            let declNameExpr = arg.expression.as(DeclReferenceExprSyntax.self),
            declNameExpr.argumentNames == nil
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
      self.diagnose(.expectedArgumentsInAttribute(node))
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
      self.diagnose(.expectedArgumentsInAttribute(node))
      return []
    }
    guard let args = args.as(BackDeployedAttributeArgumentsSyntax.self) else {
      // TODO: Diagnose.
      return []
    }

    let atLoc = self.generateSourceLoc(node.atSign)
    let range = self.generateAttrSourceRange(node)

    let platformVersions = self.generate(platformVersionList: args.platforms)
    var result: [BridgedBackDeployedAttr] = []
    for platformVersion in platformVersions {
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
  ///   ```
  func generateCDeclAttr(attribute node: AttributeSyntax) -> BridgedCDeclAttr? {
    self.generateWithLabeledExprListArguments(attribute: node) { args in
      guard let name = self.generateConsumingSimpleStringLiteralAttrOption(args: &args) else {
        return nil
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        name: name
      )
    }
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

    } else {
      // TODO: Diagnosse.
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

  /// E.g.
  ///   ```
  ///   @derivative(of: foo(arg:), wrt: self)
  ///   ```
  func generateDerivativeAttr(attribute node: AttributeSyntax) -> BridgedDerivativeAttr? {
    guard let args = node.arguments?.as(DerivativeAttributeArgumentsSyntax.self) else {
      fatalError("(compiler bug) invalid arguments for @derivative attribute")
    }
    guard let originalDecl = self.generateDerivativeOriginalDecl(expr: args.originalDeclName) else {
      return nil
    }

    let accessorKind: BridgedAccessorKind?
    if let accessorToken = args.accessorSpecifier {
      accessorKind = self.generate(accessorSpecifier: accessorToken)
    } else {
      accessorKind = nil
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
      // TODO: Diagnose.
      fatalError("not supported kind for @differentiable attribute")
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

    var visibility: BridgedAccessLevel = .none
    var metadata: BridgedStringRef? = nil

    while let arg = args.popFirst() {
      switch arg.label.rawText {
      case "visibility":
        guard visibility == .none else {
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
        case .fileprivate: visibility = .filePrivate
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
      accessLevel: visibility
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
    let effectKind: BridgedEffectsKind
    switch arguments.first!.rawText {
    case "readonly":
      effectKind = .readOnly
    case "readnone":
      effectKind = .readNone
    case "releasenone":
      effectKind = .releaseNone
    case "readwrite":
      effectKind = .readWrite
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
  func generateExclusivityAttr(attribute node: AttributeSyntax) -> BridgedExclusivityAttr? {
    let mode: BridgedExclusivityAttrMode? = self.generateSingleAttrOption(
      attribute: node,
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
      // Exposure kind.
      let kind: BridgedExposureKind? = self.generateConsumingPlainIdentifierAttrOption(args: &args) {
        switch $0.rawText {
        case "Cxx":
          return .cxx
        case "Wasn":
          return .wasm
        default:
          return nil
        }
      }
      guard let kind else {
        return nil
      }

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
    return self.generateWithLabeledExprListArguments(attribute: node) { args in
      let kind: BridgedExternKind? = self.generateConsumingPlainIdentifierAttrOption(args: &args) {
        switch $0.rawText {
        case "c":
          return .C
        case "wasm":
          return .wasm
        default:
          return nil
        }
      }
      guard let kind else {
        return nil
      }

      // Module and symbol name.
      let moduleName: BridgedStringRef?
      let symbolName: BridgedStringRef?
      switch kind {
      case .C:
        moduleName = nil
        symbolName = args.isEmpty ? nil : self.generateConsumingSimpleStringLiteralAttrOption(args: &args)
      case .wasm:
        guard let _moduleName = self.generateConsumingSimpleStringLiteralAttrOption(args: &args, label: "module") else {
          return nil
        }
        guard let _symbolName = self.generateConsumingSimpleStringLiteralAttrOption(args: &args, label: "name") else {
          return nil
        }

        moduleName = _moduleName
        symbolName = _symbolName
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        lParenLoc: self.generateSourceLoc(node.leftParen),
        rParenLoc: self.generateSourceLoc(node.rightParen),
        kind: kind,
        moduleName: moduleName ?? "",
        name: symbolName ?? ""
      )
    }
  }

  /// E.g.
  ///   ```
  ///   @_section("__TEXT,__mysection")
  ///   ```
  func generateSectionAttr(attribute node: AttributeSyntax) -> BridgedSectionAttr? {
    return self.generateWithLabeledExprListArguments(attribute: node) { args in
      guard let name = self.generateConsumingSimpleStringLiteralAttrOption(args: &args) else {
        return nil
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        name: name
      )
    }
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
  ///   @inline(never)
  ///   @inline(__always)
  ///   ```
  func generateInlineAttr(attribute node: AttributeSyntax) -> BridgedInlineAttr? {
    let kind: BridgedInlineKind? = self.generateSingleAttrOption(
      attribute: node,
      {
        switch $0.rawText {
        case "never": return .never
        case "__always": return .always
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

  func generateLifetimeDescriptor(expr node: ExprSyntax) -> BridgedLifetimeDescriptor? {
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
      declRefExpr.argumentNames == nil
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

    // TODO: Diangose
    fatalError("expected identifier, 'self', or integer in @lifetime")
  }

  func generateLifetimeEntry(attribute node: AttributeSyntax) -> BridgedLifetimeEntry? {
    self.generateWithLabeledExprListArguments(attribute: node) { args in
      guard !args.isEmpty else {
        // TODO: Diagnose
        fatalError("expected arguments in @lifetime attribute")
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
            fatalError("invalid argument label in @lifetime attribute")
          }
        }

        if let src = self.generateLifetimeDescriptor(expr: arg.expression) {
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
  ///   @lifetime(src1, src2)
  ///   @lifetime(target: borrow src1, copy src2)
  ///   @lifetime(2)
  ///   @lifetime(self)
  ///   ```
  func generateLifetimeAttr(attribute node: AttributeSyntax) -> BridgedLifetimeAttr? {
    guard let entry = self.generateLifetimeEntry(attribute: node) else {
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
    guard node.argumentNames == nil else {
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
        name = BridgedDeclNameRef.createParsed(.createIdentifier(self.ctx.getIdentifier("_")))
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
      let role = self.generateConsumingPlainIdentifierAttrOption(args: &args) {
        BridgedMacroRole(from: $0.rawText.bridged)
      }
      guard let role = role else {
        return nil
      }
      guard role != .none else {
        // TODO: Diagnose.
        return nil
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
    let modifier: BridgedNonIsolatedModifier? = self.generateSingleAttrOption(
      attribute: node,
      {
        switch $0.rawText {
        case "unsafe": return .unsafe
        case "nonsending": return .nonSending
        default: return nil
        }
      },
      valueIfOmitted: BridgedNonIsolatedModifier.none
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
    let modifier: BridgedInheritActorContextModifier? = self.generateSingleAttrOption(
      attribute: node,
      {
        switch $0.rawText {
        case "always": return .always
        default: return nil
        }
      },
      valueIfOmitted: BridgedInheritActorContextModifier.none
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
    let name: BridgedIdentifier? = self.generateSingleAttrOption(
      attribute: node,
      self.generateIdentifier,
      valueIfOmitted: BridgedIdentifier()
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
    let name: BridgedIdentifier? = self.generateSingleAttrOption(attribute: node) {
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

    let platformVersions = self.generate(platformVersionList: args.platforms)
    var result: [BridgedOriginallyDefinedInAttr] = []
    for platformVersion in platformVersions {
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
    let name = self.generateSingleAttrOption(attribute: node, { self.generateIdentifier($0) }, valueIfOmitted: BridgedIdentifier())
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

  func generateValueOrType(expr node: ExprSyntax) -> BridgedTypeRepr? {
    var node = node

    // Try value first.
    let minusLoc: BridgedSourceLoc
    if let prefixExpr = node.as(PrefixOperatorExprSyntax.self),
      prefixExpr.operator.rawText == "-",
      prefixExpr.expression.is(IntegerLiteralExprSyntax.self) {
      minusLoc = self.generateSourceLoc(prefixExpr.operator)
      node = prefixExpr.expression
    } else {
      minusLoc = nil
    }
    if let integerExpr = node.as(IntegerLiteralExprSyntax.self) {
      let value = self.copyAndStripUnderscores(text: integerExpr.literal.rawText)
      return BridgedIntegerTypeRepr.createParsed(
        self.ctx,
        string: value,
        loc: self.generateSourceLoc(node), minusLoc: minusLoc
      ).asTypeRepr
    }

    assert(!minusLoc.isValid)
    return self.generateTypeRepr(expr: node)
  }

  func generateRawLayoutAttr(attribute node: AttributeSyntax) -> BridgedRawLayoutAttr? {
    self.generateWithLabeledExprListArguments(attribute: node) { args in
      switch args.first?.label?.rawText {
      case "size":
        return generateSizeAlignment()
      case "like":
        return generateScalarLike()
      case "likeArrayOf":
        return generateArrayLike()
      default:
        // TODO: Diagnose.
        fatalError("invalid argument for @rawLayout attribute")
      }

      func generateSizeAlignment() -> BridgedRawLayoutAttr? {
        guard let size = generateConsumingIntegerLiteralOption(label: "size") else {
          // Should already be diagnosed.
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
        self.generateConsumingAttrOption(args: &args, label: label) {
          guard let integerExpr = $0.as(IntegerLiteralExprSyntax.self) else {
            // TODO: Diagnose
            fatalError("expected integer literal for '\(String(syntaxText: label)):' in @_rawLayout")
          }
          guard let count = integerExpr.representedLiteralValue else {
            fatalError("invalid value literal for '\(String(syntaxText: label)):' in @_rawLayout")
          }
          return count
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
  ///   @_specialize(exporeted: true, T == Int)
  ///   ```
  func generateSpecializeAttr(attribute node: AttributeSyntax, attrName: SyntaxText) -> BridgedSpecializeAttr? {
    guard
      var args = node.arguments?.as(SpecializeAttributeArgumentListSyntax.self)?[...]
    else {
      // TODO: Diagnose
      return nil
    }

    var exported: Bool?
    var kind: BridgedSpecializationKind? = nil
    var whereClause: BridgedTrailingWhereClause? = nil
    var targetFunction: BridgedDeclNameRef? = nil
    var spiGroups: [BridgedIdentifier] = []
    var availableAttrs: [BridgedAvailableAttr] = []

    while let arg = args.popFirst() {
      switch arg {
      case .genericWhereClause(let arg):
        whereClause =  self.generate(genericWhereClause: arg)
      case .specializeTargetFunctionArgument(let arg):
        if targetFunction != nil {
          // TODO: Diangose.
        }
        targetFunction = self.generateDeclNameRef(declReferenceExpr: arg.declName).name
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
        switch arg.label.rawText {
        case "kind":
          if kind != nil {
            // TODO: Diagnose.
          }
          switch arg.value.rawText {
          case "partial":
            kind = .partial
          case "full":
            kind = .full
          default:
            // TODO: Diagnose.
            break
          }

        case "exported":
          if exported != nil {
            // TODO: Diagnose.
          }
          switch arg.value.rawText {
          case "true":
            exported = true
          case "false":
            exported = false
          default:
            // TODO: Diagnose
            break
          }

        case "spi":
          guard arg.value.rawTokenKind == .identifier || arg.value.rawTokenKind == .wildcard else {
            // TODO: Diagnose
            break
          }
          spiGroups.append(self.generateIdentifier(arg.value))

        default:
          // TODO: Diagnose
          break
        }

      }
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      whereClause: whereClause.asNullable,
      exported: exported ?? false,
      kind: kind ?? .full,
      taretFunction: targetFunction ?? BridgedDeclNameRef(),
      spiGroups: spiGroups.lazy.bridgedArray(in: self),
      availableAttrs: availableAttrs.lazy.bridgedArray(in: self)
    )
  }

  /// E.g.:
  ///   ```
  ///   @_spi(GroupName)
  ///   ```
  func generateSPIAccessControlAttr(attribute node: AttributeSyntax) -> BridgedSPIAccessControlAttr? {
    let spiName: BridgedIdentifier? = self.generateSingleAttrOption(attribute: node) {
      self.generateIdentifier($0)
    }
    guard let spiName else {
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

      guard !args.isEmpty else {
        // TODO: Diagnostics requires arguments
        return nil
      }

      enum Argument: UInt8 {
        case initializes
        case accesses
        case invalid
      }
      var argState = AttrArgumentState<Argument, UInt8>(.invalid)
      var initializesProperties: [BridgedIdentifier] = []
      var accessesProperties: [BridgedIdentifier] = []

      while let arg = args.popFirst() {
        // Label.
        switch arg.label?.rawText {
        case "initializes":
          if argState.hasSeen(.initializes) {
            // TODO: Diagnose duplicated label.
          }
          argState.current = .initializes
        case "accesses":
          if argState.hasSeen(.accesses) {
            // TODO: Diagnose duplicated label.
          }
          argState.current = .accesses
        case nil where argState.current == .invalid:
          break
        default:
          if argState.hasSeen(.invalid) {
            // TODO: Diagnose invalid or missing label.
          }
          argState.current = .invalid
        }

        // Value.
        func generatePropertyName(expr node: ExprSyntax) -> BridgedIdentifier? {
          guard
            let node = node.as(DeclReferenceExprSyntax.self),
            node.argumentNames == nil
          else {
            // TODO: Diagnose.
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
    let name: BridgedIdentifier? = self.generateSingleAttrOption(attribute: node) {
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
  ///   @transpose(of: foo(_:), wrt: self)
  ///   ```
  func generateTransposeAttr(attribute node: AttributeSyntax) -> BridgedTransposeAttr? {
    guard let args = node.arguments?.as(DerivativeAttributeArgumentsSyntax.self) else {
      fatalError("(compiler bug) invalid arguments for @derivative attribute")
    }
    guard let originalDecl = self.generateDerivativeOriginalDecl(expr: args.originalDeclName) else {
      return nil
    }

    if let accessorToken = args.accessorSpecifier {
      // TODO: Diagnostics.
      _ = accessorToken
      fatalError("(compiler bug) unexpected accessor kind for @transpose attribute")
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
      let nameLoc = self.generateIdentifierAndSourceLoc(token)
      return BridgedUnqualifiedIdentTypeRepr.createParsed(
        self.ctx,
        loc: nameLoc.sourceLoc,
        name: nameLoc.identifier
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

    var message: BridgedStringRef? = nil
    if node.arguments != nil {
      message = self.generateWithLabeledExprListArguments(attribute: node) { args in
        self.generateConsumingSimpleStringLiteralAttrOption(args: &args, label: "message")
      }
      guard message != nil else {
        return nil
      }
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      message: self.ctx.allocateCopy(string: message ?? "")
    )
  }

  func generateUnavailableInEmbeddedAttr(attribute node: AttributeSyntax) -> BridgedAvailableAttr? {
    if ctx.langOptsHasFeature(.Embedded) {
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

  func generateSimpleDeclAttr(attribute node: AttributeSyntax, kind: BridgedDeclAttrKind) -> BridgedDeclAttribute? {
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
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      type: type,
      initContext: initContext.asNullable,
      argumentList: argList.asNullable
    )
  }

  func generateAttrSourceRange(_ node: AttributeSyntax) -> BridgedSourceRange {
    guard let firstNameTok = node.attributeName.firstToken(viewMode: .sourceAccurate) else {
      return BridgedSourceRange()
    }
    return self.generateSourceRange(start: firstNameTok, end: node.lastToken(viewMode: .sourceAccurate)!)
  }

  func generateStringLiteralTextIfNotInterpolated(expr node: some ExprSyntaxProtocol) -> BridgedStringRef? {
    if let segments = node.as(SimpleStringLiteralExprSyntax.self)?.segments {
      return extractRawText(segments).bridged
    } else if let segments = node.as(StringLiteralExprSyntax.self)?.segments,
      segments.allSatisfy({ $0.is(StringSegmentSyntax.self) })
    {
      return extractRawText(segments).bridged
    }
    // TODO: Diagnose.
    fatalError("expected string literal without interpolation")
    // return nil
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
      self.diagnose(.expectedArgumentsInAttribute(node))
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
      if let declRefExpr = $0.as(DeclReferenceExprSyntax.self), declRefExpr.argumentNames == nil {
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
    _ valueGeneratorFunction: (TokenSyntax) -> Result?,
    valueIfOmitted: Result? = nil
  ) -> Result? {
    guard node.arguments != nil else {
      if let valueIfOmitted {
        return valueIfOmitted
      }
      self.diagnose(.expectedArgumentsInAttribute(node))
      return nil
    }

    return self.generateWithLabeledExprListArguments(attribute: node) { args in
      self.generateConsumingPlainIdentifierAttrOption(
        args: &args,
        valueGeneratorFunction
      )
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
      return self.generateAccessControlAttr(declModifier: node, level: .filePrivate)
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
      let kind = BridgedDeclAttrKind(from: node.name.rawText.bridged)
      guard kind != .none else {
        // TODO: Diagnose.
        fatalError("(compiler bug) unknown decl modifier")
      }
      if !BridgedDeclAttribute.isDeclModifier(kind) {
        // TODO: Diagnose.
        fatalError("(compiler bug) decl attribute was parsed as a modifier")
      }
      return self.generateSimpleDeclAttr(declModifier: node, kind: kind)
    }
  }

  func generateAccessControlAttr(declModifier node: DeclModifierSyntax, level: BridgedAccessLevel)
    -> BridgedDeclAttribute?
  {
    if let detail = node.detail {
      guard detail.detail.rawText == "set" else {
        // TODO: Diagnose
        fatalError("only accepted modifier argument is '(set)'")
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
    let modifier: BridgedNonIsolatedModifier
    switch node.detail?.detail.rawText {
    case "unsafe":
      modifier = .unsafe
    case "nonsending":
      modifier = .nonSending
    case nil:
      modifier = .none
    case let text?:
      // TODO: Diagnose
      _ = text
      fatalError("invalid argument for nonisolated modifier")
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
      guard node.detail == nil else {
        // TODO: Diagnose.
        fatalError("invalid argument for 'weak' modifier")
      }
    case .unowned:
      switch node.detail?.detail.rawText {
      case "safe", nil:
        kind = .unowned
      case "unsafe":
        kind = .unmanaged
      case let text?:
        // TODO: Diagnose
        _ = text
        fatalError("invalid argument for 'unowned' modifier")
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

  func generateSimpleDeclAttr(declModifier node: DeclModifierSyntax, kind: BridgedDeclAttrKind) -> BridgedDeclAttribute? {
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
