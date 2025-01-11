//===--- Attrs.swift ------------------------------------------------------===//
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

@_spi(ExperimentalLanguageFeatures) @_spi(RawSyntax) import SwiftSyntax

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
    if let firstTok = node.firstToken(viewMode: .sourceAccurate) {
      var pos = firstTok.position
      for p in firstTok.leadingTrivia {
        switch p {
        case .docLineComment, .docBlockComment:
          let range = self.generateCharSourceRange(start: pos, length: p.sourceLength)
          let attr = BridgedRawDocCommentAttr.createParsed(self.ctx, range: range)
          attrs.add(attr.asDeclAttribute)
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
        return self.generateAvailableAttr(attribute: node).forEach { handle($0.asDeclAttribute) }
      case .backDeployed:
        fatalError("unimplemented")
      case .cDecl:
        return handle(self.generateCDeclAttr(attribute: node)?.asDeclAttribute)
      case .derivative:
        fatalError("unimplemented")
      case .differentiable:
        fatalError("unimplemented")
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
        fatalError("unimplemented")
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
        // FIXME: handle multiple results.
        return handle(self.generateOriginallyDefinedInAttr(attribute: node).first?.asDeclAttribute)
      case .privateImport:
        return handle(self.generatePrivateImportAttr(attribute: node)?.asDeclAttribute)
      case .projectedValueProperty:
        return handle(self.generateProjectedValuePropertyAttr(attribute: node)?.asDeclAttribute)
      case .rawLayout:
        fatalError("unimplemented")
      case .safe:
        fatalError("unimplemented")
      case .section:
        return handle(self.generateSectionAttr(attribute: node)?.asDeclAttribute)
      case .semantics:
        return handle(self.generateSemanticsAttr(attribute: node)?.asDeclAttribute)
      case .silGenName:
        return handle(self.generateSILGenNameAttr(attribute: node)?.asDeclAttribute)
      case .specialize:
        return handle(self.generateSpecializeAttr(attribute: node)?.asDeclAttribute)
      case .spiAccessControl:
        return handle(self.generateSPIAccessControlAttr(attribute: node)?.asDeclAttribute)
      case .storageRestrictions:
        return handle(self.generateStorageRestrictionAttr(attribute: node)?.asDeclAttribute)
      case .swiftNativeObjCRuntimeBase:
        return handle(self.generateSwiftNativeObjCRuntimeBaseAttr(attribute: node)?.asDeclAttribute)
      case .transpose:
        fatalError("unimplemented")
      case .typeEraser:
        fatalError("unimplemented")
      case .unavailableFromAsync:
        return handle(self.generateUnavailableFromAsyncAttr(attribute: node)?.asDeclAttribute)
      case .coroutine:
        fatalError("unimplemented")

      // Simple attributes.
      case .addressableSelf,
        .addressableForDependencies,
        .alwaysEmitConformanceMetadata,
        .alwaysEmitIntoClient,
        .atReasync,
        .atRethrows,
        .borrowed,
        .compilerInitialized,
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
        .inheritActorContext,
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
        fatalError("unimplemented")
      case .nonisolated:
        // TODO: Diagnose.
        return handle(self.generateNonisolatedAttr(attribute: node)?.asDeclAttribute)
      case .referenceOwnership:
        // TODO: Diagnose.
        return handle(self.generateReferenceOwnershipAttr(attribute: node, attrName: attrName)?.asDeclAttribute)
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
        .compileTimeConst:

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
        .setterAccess,
        .rethrows,
        .reasync:
        // TODO: Diagnose or fallback to custom attributes?
        return

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
      abiDecl = self.generate(variableDecl: varDecl).asDecl
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
    guard
      let arg = node.arguments?.as(TokenSyntax.self)
    else {
      print("Not a token")
      // TODO: Diagnose.
      return nil
    }
    let value: Int? = Int(String(syntaxText: arg.rawText))
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
  ///   @_cdecl("c_function_name")
  ///   ```
  func generateCDeclAttr(attribute node: AttributeSyntax) -> BridgedCDeclAttr? {
    guard
      // `@_cdecl` attribute has `.string(StringLiteralExprSyntax)` arguments.
      let arg = node.arguments?.as(StringLiteralExprSyntax.self)
    else {
      // TODO: Diagnose.
      return nil
    }
    guard
      let name = self.generateStringLiteralTextIfNotInterpolated(expr: arg)
    else {
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

  /// E.g.:
  ///   ```
  ///   @available(macOS 10.12, iOS: 13, *)
  ///   @available(macOS, introduced: 10.12)
  ///   ```
  func generateAvailableAttr(attribute node: AttributeSyntax) -> [BridgedAvailableAttr] {
    guard
      // `@available` has special argument list syntax.
      let args = node.arguments?.as(AvailabilityArgumentListSyntax.self)
    else {
      // TODO: Diagnose.
      return []
    }

    _ = args
    fatalError("unimplemented")
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
      let name = self.generateConsumingSimpleStringLiteralAttrOption(args: &args) ?? ""

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        range: self.generateAttrSourceRange(node),
        name: name,
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
      guard let arg = arg.as(DeclReferenceExprSyntax.self) else {
        // TODO: Diagnose.
        return nil
      }
      name = self.generateDeclNameRef(declReferenceExpr: arg).name
      if arguments.count >= 2 {
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
    let isUnsafe = self.generateSingleAttrOption(
      attribute: node,
      {
        switch $0.rawText {
        case "unsafe":
          return true
        default:
          // FIXME: Diagnose.
          return nil
        }
      },
      valueIfOmitted: false
    )
    guard let isUnsafe else {
      return nil
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      isUnsafe: isUnsafe
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

  func generateOriginallyDefinedInAttr(attribute node: AttributeSyntax) -> [BridgedOriginallyDefinedInAttr] {
    guard
      // `@_OriginallyDefinedIn` has special argument list syntax.
      let args = node.arguments?.as(OriginallyDefinedInAttributeArgumentsSyntax.self)
    else {
      // TODO: Diagnose.
      return []
    }

    _ = args
    fatalError("unimplemented")
  }

  func generatePrivateImportAttr(attribute node: AttributeSyntax) -> BridgedPrivateImportAttr? {
    guard
      // `@_private` has special argument list syntax
      let args = node.arguments?.as(UnderscorePrivateAttributeArgumentsSyntax.self)
    else {
      // TODO: Diagnose
      return nil
    }

    guard let fileName = self.generateStringLiteralTextIfNotInterpolated(expr: args.filename) else {
      // TODO: Diagnose
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
    guard
      let arg = node.arguments?.as(StringLiteralExprSyntax.self)
    else {
      // TODO: Diagnose.
      return nil
    }
    guard
      let value = self.generateStringLiteralTextIfNotInterpolated(expr: arg)
    else {
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
  func generateSpecializeAttr(attribute node: AttributeSyntax) -> BridgedSpecializeAttr? {
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
        // TODO: Implement
        _ = arg
        _ = availableAttrs = availableAttrs
        fatalError("unimplemented")
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
  ///   @_unavailableFromAsync
  ///   @_unavailableFromAsync(message: "use fooBar(_:) instead")
  ///   ```
  func generateUnavailableFromAsyncAttr(attribute node: AttributeSyntax) -> BridgedUnavailableFromAsyncAttr? {
    var message: BridgedStringRef? = nil
    if node.arguments != nil {
      // FIXME: Should be normal LabeledExprListSyntax arguments.

      guard let args = node.arguments?.as(UnavailableFromAsyncAttributeArgumentsSyntax.self) else {
        // TODO: Diagnose.
        return nil
      }
      message = self.generateStringLiteralTextIfNotInterpolated(expr: args.message)
    }
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      message: self.ctx.allocateCopy(string: message ?? "")
    )
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
      guard
        let declRefExpr = $0.as(DeclReferenceExprSyntax.self),
        declRefExpr.argumentNames == nil
      else {
        // TODO: Diagnose.
        return nil
      }
      return valueGeneratorFunction(declRefExpr.baseName)
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
    guard node.leftParen != nil, let arguments = node.arguments else {
      if let valueIfOmitted {
        return valueIfOmitted
      }
      self.diagnose(.expectedArgumentsInAttribute(node))
      return nil
    }

    if case .token(let tok) = arguments {
      // Special case: was parsed as a token, not an an argument list
      return valueGeneratorFunction(tok)
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
    case .weak, .unowned:
      return self.generateReferenceOwnershipAttr(declModifier: node)?.asDeclAttribute
    default:
      // Other modifiers are all "simple" attributes.
      let kind = BridgedDeclAttrKind(from: node.name.rawText.bridged)
      guard kind != .none else {
        // TODO: Diagnose?
        assertionFailure("unknown decl modifier")
        return nil
      }
      return self.generateSimpleDeclAttr(declModifier: node, kind: kind)
    }
  }

  func generateAccessControlAttr(declModifier node: DeclModifierSyntax, level: BridgedAccessLevel)
    -> BridgedDeclAttribute?
  {
    if let detail = node.detail {
      precondition(detail.detail.keywordKind == .set, "only accepted modifier argument is '(set)'")
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
        return nil
      }
    case .unowned:
      switch node.detail?.detail.keywordKind {
      case .safe, nil:
        kind = .unowned
      case .unsafe:
        kind = .unmanaged
      case _?:
        // TODO: Diagnose
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

/// Simpler helper for handling attribute arguments in "generate" functions.
private struct AttrArgumentState<Flag: RawRepresentable, SeenStorage: FixedWidthInteger> where Flag.RawValue: FixedWidthInteger {
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
