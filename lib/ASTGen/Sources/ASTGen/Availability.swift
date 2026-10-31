//===--- Availability.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import SwiftDiagnostics
import SwiftParserDiagnostics
import SwiftIfConfig
@_spi(Compiler) import SwiftParser
@_spi(RawSyntax) @_spi(Compiler) import SwiftSyntax

extension ASTGenVisitor {
  /// Implementation detail for `generateAvailableAttr(attribute:)` and `generateSpecializeAttr(attribute:)`.
  func generateAvailableAttr(
    atLoc: SourceLoc,
    range: SourceRange,
    attrName: SyntaxText,
    args: AvailabilityArgumentListSyntax
  ) -> [BridgedAvailableAttr] {

    let isSPI = attrName == "_spi_available"

    // Check if this is "shorthand" syntax.
    if let firstArg = args.first?.argument {
      // We need to check availability macros specified by '-define-availability'.
      let isShorthand: Bool
      if let firstToken = firstArg.as(TokenSyntax.self), firstToken.rawTokenKind == .identifier, peekAvailabilityMacroName(firstToken.rawText) {
        //   @available(myFeature, *)
        isShorthand = true
      } else if firstArg.is(PlatformVersionSyntax.self) {
        //   @available(myPlatform 2.7, *)
        isShorthand = true
      } else {
        isShorthand = false
      }
      if isShorthand {
        return self.generateAvailableAttrShorthand(atLoc: atLoc, range: range, args: args, isSPI: isSPI)
      }
    }

    // E.g.
    //   @available(macOS, introduced: 10.12, deprecated: 11.2)
    //   @available(*, unavailable, message: "out of service")
    let attr = self.generateAvailableAttrExtended(
      atLoc: atLoc,
      range: range,
      attrName: attrName,
      args: args,
      isSPI: isSPI
    )
    if let attr {
      return [attr]
    } else {
      return []
    }
  }

  func generate(versionTuple node: VersionTupleSyntax?) -> VersionTuple? {
    guard let node, let tuple = VersionTuple(parsing: node.trimmedDescription) else {
      return nil
    }
    return tuple
  }

  func generateAvailableAttrShorthand(
    atLoc: SourceLoc,
    range: SourceRange,
    args: AvailabilityArgumentListSyntax,
    isSPI: Bool
  ) -> [BridgedAvailableAttr] {
    let specs = self.generateAvailabilitySpecList(args: args, context: .availableAttr)

    var isFirst = true
    var result: [BridgedAvailableAttr] = []
    let containsWildCard = specs.contains { $0.isWildcard }
    for spec in specs {
      guard !spec.isWildcard else {
        continue
      }

      let domainOrIdentifier = spec.domainOrIdentifier
      // The domain should not be resolved during parsing.
      assert(!domainOrIdentifier.isDomain)
      if domainOrIdentifier.asIdentifier == nil {
        continue
      }

      // TODO: Assert 'spec' is domain identifier.
      let attr = BridgedAvailableAttr.createParsed(
        self.ctx,
        atLoc: atLoc,
        range: range,
        domainIdentifier: domainOrIdentifier.asIdentifier,
        domainLoc: spec.sourceRange.start,
        kind: .default,
        message: BridgedStringRef(),
        renamed: BridgedStringRef(),
        introduced: spec.rawVersion,
        introducedRange: spec.versionRange,
        deprecated: BridgedVersionTuple(),
        deprecatedRange: SourceRange(),
        obsoleted: BridgedVersionTuple(),
        obsoletedRange: SourceRange(),
        isSPI: isSPI
      )
      attr.setMacroLoc(spec.macroLoc)
      attr.setIsGroupMember()
      if containsWildCard {
        attr.setIsGroupedWithWildcard()
      }
      if isFirst {
        attr.setIsGroupTerminator()
        isFirst = false
      }

      result.append(attr)
    }
    return result
  }

  /// The source spelling of an availability attribute kind, for diagnostics.
  func attrKindSpelling(_ kind: BridgedAvailableAttrKind) -> String {
    switch kind {
    case .default: return "available"
    case .deprecated: return "deprecated"
    case .unavailable: return "unavailable"
    case .noAsync: return "noasync"
    @unknown default: return "available"
    }
  }

  func generateAvailableAttrExtended(
    atLoc: SourceLoc,
    range: SourceRange,
    attrName: SyntaxText,
    args: AvailabilityArgumentListSyntax,
    isSPI: Bool
  ) -> BridgedAvailableAttr? {
    var args = args[...]
    let attrNameText = String(syntaxText: attrName)

    // The platfrom.
    guard let platformToken = args.popFirst()?.argument.as(TokenSyntax.self) else {
      self.diagnose(.attr_availability_expected_platform(attrNameText), at: range.start)
      return nil
    }
    let domain = self.generateIdentifierAndSourceLoc(platformToken)

    // Other arguments can be shuffled.
    enum Argument: UInt8 {
      case message
      case renamed
      case introduced
      case deprecated
      case obsoleted
      case invalid
    }
    var argState = AttrArgumentState<Argument, UInt8>(.invalid)
    var attrKind: BridgedAvailableAttrKind = .default

    struct VersionAndRange {
      let version: VersionTuple
      let range: SourceRange
    }

    var introduced: VersionAndRange? = nil
    var deprecated: VersionAndRange? = nil
    var obsoleted: VersionAndRange? = nil
    var message: BridgedStringRef? = nil
    var renamed: BridgedStringRef? = nil

    /// Record `newKind`, diagnosing if a conflicting kind was already given.
    func setAttrKind(_ newKind: BridgedAvailableAttrKind, _ spelling: String, at token: TokenSyntax) {
      guard attrKind == .default else {
        self.diagnose(
          .attr_availability_multiple_kinds(attrNameText, spelling, attrKindSpelling(attrKind)),
          at: token
        )
        return
      }
      attrKind = newKind
    }

    func generateVersion(arg: AvailabilityLabeledArgumentSyntax, into target: inout VersionAndRange?) {
      guard
        let versionSytnax = arg.value.as(VersionTupleSyntax.self),
        let version = VersionTuple(parsing: versionSytnax.trimmedDescription)
      else {
        self.diagnose(.attr_availability_expected_version(attrNameText), at: arg.value)
        return
      }
      if target != nil {
        self.diagnose(
          .attr_availability_invalid_duplicate(String(syntaxText: arg.label.rawText)),
          at: arg.label
        )
        return
      }

      target = .init(version: version, range: self.generateSourceRange(versionSytnax))
    }

    /// Generate a string-literal argument, diagnosing the ways it can be
    /// ill-formed. Returns nil if it was not usable.
    func generateStringArgument(
      arg: AvailabilityLabeledArgumentSyntax,
      into target: inout BridgedStringRef?
    ) {
      let label = String(syntaxText: arg.label.rawText)
      guard let literal = arg.value.as(SimpleStringLiteralExprSyntax.self) else {
        self.diagnose(.attr_expected_string_literal(attrNameText), at: arg.value)
        return
      }
      guard let text = self.generateStringLiteralTextIfNotInterpolated(expr: literal) else {
        // `generateStringLiteralTextIfNotInterpolated` has diagnosed.
        return
      }
      guard target == nil else {
        self.diagnose(.attr_availability_invalid_duplicate(label), at: arg.label)
        return
      }
      target = text
    }

    while let arg = args.popFirst() {
      switch arg.argument {
      case .availabilityVersionRestriction(let platformVersion):
        // E.g. `@available(EnabledDomain, macOS 10.10, *)`: shorthand
        // specifications cannot follow a platform given in the extended form.
        self.diagnose(
          .attr_availability_expected_option(attrNameText),
          at: platformVersion
        )

      case .token(let arg):
        // 'deprecated', 'unavailable, 'noasync' changes the mode.
        switch arg.rawText {
        case "deprecated":
          setAttrKind(.deprecated, "deprecated", at: arg)
        case "unavailable":
          setAttrKind(.unavailable, "unavailable", at: arg)
        case "noasync":
          setAttrKind(.noAsync, "noasync", at: arg)
        case "*":
          // A wildcard is only meaningful in the shorthand form; the C++ parser
          // reports it as an unexpected option here.
          self.diagnose(.attr_availability_expected_option(attrNameText), at: arg)
        default:
          self.diagnose(.attr_availability_expected_option(attrNameText), at: arg)
        }

      case .availabilityLabeledArgument(let arg):
        switch arg.label.rawText {
        case "message":
          argState.current = .message
        case "renamed":
          argState.current = .renamed
        case "introduced":
          argState.current = .introduced
        case "deprecated":
          argState.current = .deprecated
        case "obsoleted":
          argState.current = .obsoleted
        default:
          argState.current = .invalid
        }

        switch argState.current {
        case .introduced:
          generateVersion(arg: arg, into: &introduced)
        case .deprecated:
          generateVersion(arg: arg, into: &deprecated)
        case .obsoleted:
          generateVersion(arg: arg, into: &obsoleted)
        case .message:
          generateStringArgument(arg: arg, into: &message)
        case .renamed:
          generateStringArgument(arg: arg, into: &renamed)
        case .invalid:
          self.diagnose(.attr_availability_expected_option(attrNameText), at: arg.label)
        }
      }
    }

    return BridgedAvailableAttr.createParsed(
      self.ctx,
      atLoc: atLoc,
      range: range,
      domainIdentifier: domain.identifier,
      domainLoc: domain.sourceLoc,
      kind: attrKind,
      message: message ?? BridgedStringRef(),
      renamed: renamed ?? BridgedStringRef(),
      introduced: introduced?.version.bridged ?? BridgedVersionTuple(),
      introducedRange: introduced?.range ?? SourceRange(),
      deprecated: deprecated?.version.bridged ?? BridgedVersionTuple(),
      deprecatedRange: deprecated?.range ?? SourceRange(),
      obsoleted: obsoleted?.version.bridged ?? BridgedVersionTuple(),
      obsoletedRange: obsoleted?.range ?? SourceRange(),
      isSPI: isSPI
    )
  }

  /// Return true if 'name' is an availability macro name.
  func peekAvailabilityMacroName(_ name: SyntaxText) -> Bool {
    let map = ctx.availabilityMacroMap
    return map.has(name: name.bridged)
  }

  func generate(availabilityMacroDefinition node: AvailabilityMacroDefinitionFileSyntax) -> BridgedAvailabilityMacroDefinition {

    let name = allocateBridgedString(node.platformVersion.platform.text)
    let version = self.generate(versionTuple: node.platformVersion.version)
    let specs = self.generateAvailabilitySpecList(args: node.specs, context: .macro)

    let specsBuffer = UnsafeMutableBufferPointer<BridgedAvailabilitySpec>.allocate(capacity: specs.count)
    _ = specsBuffer.initialize(from: specs)

    return BridgedAvailabilityMacroDefinition(
      name: name,
      version: version?.bridged ?? BridgedVersionTuple(),
      specs: BridgedArrayRef(data: UnsafeRawPointer(specsBuffer.baseAddress), count: specsBuffer.count)
    )
  }

  enum AvailabilitySpecListContext {
    case availableAttr
    case poundAvailable
    case poundUnavailable
    case macro
  }

  func generateAvailabilitySpecList(args node: AvailabilityArgumentListSyntax, context: AvailabilitySpecListContext) -> [BridgedAvailabilitySpec] {
    var result: [BridgedAvailabilitySpec] = []

    func handle(domainNode: TokenSyntax, versionNode: VersionTupleSyntax?) {
      let version = self.generate(versionTuple: versionNode)
      let versionRange = self.generateSourceRange(versionNode)

      if context != .macro {
        // Try expand macro first.
        let expanded = ctx.availabilityMacroMap.get(
          name: domainNode.rawText.bridged,
          version: version?.bridged ?? BridgedVersionTuple()
        )
        if !expanded.isEmpty {
          let macroLoc = self.generateSourceLoc(domainNode)
          expanded.withElements(ofType: UnsafeRawPointer.self) { buffer in
            for ptr in buffer {
              // Make a copy of the specs to add the macro source location
              // for the diagnostic about the use of macros in inlinable code.
              let spec = BridgedAvailabilitySpec(raw: UnsafeMutableRawPointer(mutating: ptr))
                .clone(self.ctx)
              spec.setMacroLoc(macroLoc)
              result.append(spec)
            }
          }
          return
        }
      }

      // Not a macro, so the domain is an identifier to be resolved during type
      // checking.
      //
      // Deliberately does *not* require a version here. A custom availability
      // domain has no version, and a versioned domain that is missing one is
      // diagnosed after the domain resolves, by
      // `diag::avail_query_expected_version_number` in `MiscDiagnostics.cpp`.
      // The C++ parser behaves the same way: `Parser::parseAvailabilitySpec`
      // only parses a version if the next token is a number, and otherwise
      // records an empty one.
      guard domainNode.presence == .present else {
        // A missing domain token means the parser already recovered and
        // diagnosed; don't add a spec with an empty name for Sema to resolve.
        return
      }
      let platform = self.generateIdentifierAndSourceLoc(domainNode)
      // FIXME: Wasting ASTContext memory.
      // 'AvailabilitySpec' is 'ASTAllocated' but created spec is ephemeral in context of `@available` attributes.
      let spec = BridgedAvailabilitySpec.createForDomainIdentifier(
        self.ctx,
        name: platform.identifier,
        nameLoc: platform.sourceLoc,
        version: version?.bridged ?? BridgedVersionTuple(),
        versionRange: versionRange
      )
      result.append(spec)
    }

    /// The most recent shorthand specification, e.g. the `OSX 10.0` in
    /// `@available(OSX 10.0, ...)`. Used to diagnose a labeled argument that
    /// follows one, which is not allowed.
    var lastShorthand: PlatformVersionSyntax? = nil

    for parsed in node {
      switch parsed.argument {
      case .token(let tok) where tok.rawText == "*":
        let spec = BridgedAvailabilitySpec.createWildcard(
          self.ctx,
          loc: self.generateSourceLoc(tok)
        )
        result.append(spec)
      case .token(let tok):
        handle(domainNode: tok, versionNode: nil)
      case .availabilityVersionRestriction(let platformVersion):
        lastShorthand = platformVersion
        handle(domainNode: platformVersion.platform, versionNode: platformVersion.version)
      case .availabilityLabeledArgument(let arg):
        // E.g. `@available(OSX 10.0, deprecated: 10.12)`. A labeled argument
        // cannot be mixed with the shorthand form; the two spellings of
        // `@available` are parsed by different paths.
        guard let lastShorthand else {
          // No preceding shorthand to blame, so this is a malformed argument
          // list that the parser has already diagnosed.
          continue
        }
        let label = String(syntaxText: arg.label.rawText)
        var fixIts: [CompilerDiagnosticFixIt] = []
        // Suggest turning `OSX 10.0, introduced: ...` into
        // `OSX, introduced: 10.0`, by inserting the label after the platform.
        let insertion = self.generateCharSourceRange(
          start: lastShorthand.platform.endPositionBeforeTrailingTrivia,
          length: SourceLength(utf8Length: 0)
        )
        if label == "introduced" || label == "deprecated" {
          fixIts.append(.init(replace: insertion, with: ", introduced:"))
        }
        self.diagnose(
          .avail_query_argument_and_shorthand_mix_not_allowed(
            label,
            lastShorthand.trimmedDescription
          ),
          at: arg.label
        )
        if !fixIts.isEmpty {
          // The C++ parser attaches this note to the shorthand specification,
          // not to the offending label.
          self.diagnose(.avail_query_meant_introduced, at: lastShorthand, fixIts: fixIts)
        }
      }
    }

    return result
  }

  typealias GeneratedPlatformVersion = (platform: swift.PlatformKind, version: BridgedVersionTuple)

  struct GeneratedPlatformVersionList {
    var versions: [GeneratedPlatformVersion] = []

    /// Whether an entry was dropped for naming something that is not a platform
    /// -- an unrecognized name, or a macro that expanded to just a wildcard.
    ///
    /// When this is true the caller must *not* report
    /// `diag::attr_availability_need_platform_version` even if `versions` is
    /// empty: a diagnostic was already emitted for the dropped entry, and the
    /// C++ parser suppresses the second one the same way (via its
    /// `EmptyPlatformAndVersions` flag).
    var droppedNonPlatform = false

    /// Whether the caller should report that no platform version was given.
    var needsPlatformVersionDiagnostic: Bool {
      versions.isEmpty && !droppedNonPlatform
    }
  }

  /// Generate the platform-version list of an attribute such as
  /// `@backDeployed(before:)` or `@_originallyDefinedIn(module:_:)`.
  func generate(
    platformVersionList node: PlatformVersionItemListSyntax,
    attrName: String
  ) -> GeneratedPlatformVersionList {
    var result = GeneratedPlatformVersionList()

    for element in node {
      let platformVersionNode = element.platformVersion
      let platformToken = platformVersionNode.platform
      let platformName = platformToken.rawText
      let version = self.generate(versionTuple: platformVersionNode.version)?.bridged ?? BridgedVersionTuple()

      // A missing platform token is a parser recovery artifact, e.g. from a
      // trailing comma; the parser has already diagnosed it.
      guard platformToken.presence == .present else {
        result.droppedNonPlatform = true
        continue
      }

      // If the name is a platform name, use it.
      //
      // `*` is not a platform: wildcards are not meaningful in this kind of
      // list. Note this deliberately does not set `droppedNonPlatform`,
      // matching the C++ parser, which still reports an otherwise-empty list as
      // needing a platform version.
      if platformName == "*" {
        self.diagnose(.attr_availability_wildcard_ignored(attrName), at: platformToken)
        continue
      }

      // Only the major and minor version numbers are used.
      if let versionNode = platformVersionNode.version, versionNode.components.count > 1 {
        self.diagnose(
          .attr_availability_platform_version_major_minor_only(attrName),
          at: versionNode
        )
      }

      let platform = BridgedOptionalPlatformKind(from: platformName.bridged)
      if platform.hasValue {
        result.versions.append((platform: platform.value, version: version))
        continue
      }

      // If there's matching macro, use it.
      let expanded = ctx.availabilityMacroMap.get(
        name: platformName.bridged,
        version: version
      )
      if !expanded.isEmpty {
        var specs: [BridgedAvailabilitySpec] = []
        expanded.withElements(ofType: UnsafeRawPointer.self) { buffer in
          specs = buffer.map {
            BridgedAvailabilitySpec(raw: UnsafeMutableRawPointer(mutating: $0))
          }
        }

        // A macro that expands to just a wildcard contributes no platform
        // version, but is not an error either. The C++ parser special-cases
        // exactly this shape; the general loop below would instead fall through
        // to `attr_availability_need_platform_version`.
        if specs.count == 1, specs[0].isWildcard {
          result.droppedNonPlatform = true
          continue
        }

        for spec in specs {
          // A spec that came from the macro map may carry a resolved domain
          // rather than an identifier -- a wildcard is one -- so ask for the
          // platform in a way that accepts either.
          let platform = BridgedOptionalPlatformKind(from: spec.domainOrIdentifier)
          guard platform.hasValue else {
            continue
          }
          result.versions.append((platform: platform.value, version: spec.rawVersion))
        }
        continue
      }

      // Neither a platform nor a macro. Warn, with a spelling suggestion if one
      // is close enough, and skip the entry -- the C++ parser also only warns.
      result.droppedNonPlatform = true
      let corrected = PlatformKind_closestCorrectedString(platformName.bridged)
      if corrected.count > 0 {
        let correctedText = String(bridged: corrected)
        self.diagnose(
          .attr_availability_suggest_platform(
            String(syntaxText: platformName),
            attrName,
            correctedText
          ),
          at: platformToken,
          fixIts: [
            .init(replace: self.generateCharSourceRange(platformToken), with: correctedText)
          ]
        )
      } else {
        self.diagnose(
          .attr_availability_unknown_platform(String(syntaxText: platformName), attrName),
          at: platformToken
        )
      }
    }
    return result
  }

  func generate(availabilityCondition node: AvailabilityConditionSyntax) -> BridgedPoundAvailableInfo {
    let specListContext: AvailabilitySpecListContext
    switch node.availabilityKeyword.rawText {
    case "#available":
      specListContext = .poundAvailable
    case "#unavailable":
      specListContext = .poundUnavailable
    default:
      // The grammar admits only these two spellings for an
      // AvailabilityConditionSyntax keyword, so anything else is a swift-syntax
      // invariant violation rather than bad user input.
      preconditionFailure(
        "AvailabilityConditionSyntax keyword is neither '#available' nor '#unavailable'"
      )
    }
    let specs = self.generateAvailabilitySpecList(
      args: node.availabilityArguments,
      context: specListContext
    )

    return .createParsed(
      self.ctx,
      poundLoc: self.generateSourceLoc(node.availabilityKeyword),
      lParenLoc: self.generateSourceLoc(node.leftParen),
      specs: specs.lazy.bridgedArray(in: self),
      rParenLoc: self.generateSourceLoc(node.rightParen),
      isUnavailable: specListContext == .poundUnavailable
    )
  }

}

/// Parse an argument for '-define-availability' compiler option.
@_cdecl("swift_ASTGen_parseAvailabilityMacroDefinition")
func parseAvailabilityMacroDefinition(
  ctx: BridgedASTContext,
  dc: BridgedDeclContext,
  diagEngine: BridgedDiagnosticEngine,
  buffer: BridgedStringRef,
  outPtr: UnsafeMutablePointer<BridgedAvailabilityMacroDefinition>
) -> Bool {
  let buffer = UnsafeBufferPointer(start: buffer.data, count: buffer.count)

  // Parse.
  var parser = Parser(buffer)
  let parsed = AvailabilityMacroDefinitionFileSyntax.parse(from: &parser)

  // Emit diagnostics.
  let diagnostics = ParseDiagnosticsGenerator.diagnostics(for: parsed)
  for diagnostic in diagnostics {
    emitDiagnostic(
      diagnosticEngine: diagEngine,
      sourceFileBuffer: buffer,
      diagnostic: diagnostic,
      diagnosticSeverity: diagnostic.diagMessage.severity
    )
  }
  if parsed.hasError {
    return true
  }

  // Generate.
  let config = CompilerBuildConfiguration(ctx: ctx, sourceBuffer: buffer)
  let configuredRegions = parsed.configuredRegions(in: config)

  // FIXME: 'declContext' and 'configuredRegions' are never used.
  let generator = ASTGenVisitor(
    diagnosticEngine: diagEngine,
    sourceBuffer: buffer,
    declContext: dc,
    astContext: ctx,
    configuredRegions: configuredRegions
  )
  let generated = generator.generate(availabilityMacroDefinition: parsed)
  outPtr.pointee = generated
  return false
}

@_cdecl("swift_ASTGen_freeAvailabilityMacroDefinition")
func freeAvailabilityMacroDefinition(
  defintion: UnsafeMutablePointer<BridgedAvailabilityMacroDefinition>
) {
  freeBridgedString(bridged: defintion.pointee.name)

  let specs = defintion.pointee.specs
  let specsBuffer = UnsafeMutableBufferPointer(
    start: UnsafeMutablePointer(mutating: specs.data?.assumingMemoryBound(to: BridgedAvailabilitySpec.self)),
    count: specs.count
  )
  specsBuffer.deinitialize()
  specsBuffer.deallocate()
}
