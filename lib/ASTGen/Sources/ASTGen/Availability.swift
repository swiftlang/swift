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
  /// E.g.:
  ///   ```
  ///   @available(macOS 10.12, iOS 13, *)
  ///   @available(macOS, introduced: 10.12)
  ///   ```
  func generateAvailableAttr(attribute node: AttributeSyntax) -> [BridgedAvailableAttr] {
    guard let args = node.arguments else {
      self.diagnose(.expectedArgumentsInAttribute(node))
      return []
    }
    guard let args = args.as(AvailabilityArgumentListSyntax.self) else {
      // TODO: Diagnose.
      return []
    }

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
        return self.generateAvailableAttrShorthand(attribute: node)
      }
    }

    // E.g.
    //   @available(macOS, introduced: 10.12, deprecated: 11.2)
    //   @available(*, unavailable, message: "out of service")
    if let generated =  self.generateAvailableAttrExtended(attribute: node) {
      return [generated]
    }
    return []
  }

  func generate(versionTuple node: VersionTupleSyntax?) -> VersionTuple? {
    guard let node, let tuple = VersionTuple(parsing: node.trimmedDescription) else {
      return nil
    }
    return tuple
  }

  func generateAvailableAttrShorthand(attribute node: AttributeSyntax) -> [BridgedAvailableAttr] {
    let atLoc = self.generateSourceLoc(node.atSign)
    let range = self.generateAttrSourceRange(node)
    let specs = self.generateAvailabilitySpecList(
      args: node.arguments!.cast(AvailabilityArgumentListSyntax.self),
      context: .availableAttr
    )

    var result: [BridgedAvailableAttr] = []
    for spec in specs {
      let domain = spec.domain
      guard !domain.isNull() else {
        continue
      }
      let attr = BridgedAvailableAttr.createParsed(
        self.ctx,
        atLoc: atLoc,
        range: range,
        domain: domain,
        domainLoc: spec.sourceRange.start,
        kind: .default,
        message: BridgedStringRef(),
        renamed: BridgedStringRef(),
        introduced: spec.version,
        introducedRange: spec.versionRange,
        deprecated: BridgedVersionTuple(),
        deprecatedRange: BridgedSourceRange(),
        obsoleted: BridgedVersionTuple(),
        obsoletedRange: BridgedSourceRange()
      )
      result.append(attr)
    }
    return result
  }

  func generateAvailableAttrExtended(attribute node: AttributeSyntax) -> BridgedAvailableAttr? {
    var args = node.arguments!.cast(AvailabilityArgumentListSyntax.self)[...]

    // The platfrom.
    guard let platformToken = args.popFirst()?.argument.as(TokenSyntax.self) else {
      // TODO: Diangose
      fatalError("missing first arg")
    }
    let platformStr = platformToken.rawText
    let platformLoc = self.generateSourceLoc(platformToken)

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
    var attrKind: BridgedAvailableAttrKind = .default {
      willSet {
        if attrKind != .default {
          fatalError("resetting attribute kind")
        }
      }
    }

    struct VersionAndRange {
      let version: VersionTuple
      let range: BridgedSourceRange
    }

    var introduced: VersionAndRange? = nil
    var deprecated: VersionAndRange? = nil
    var obsoleted: VersionAndRange? = nil
    var message: BridgedStringRef? = nil
    var renamed: BridgedStringRef? = nil

    func generateVersion(arg: AvailabilityLabeledArgumentSyntax, into target: inout VersionAndRange?) {
      guard let versionSytnax = arg.value.as(VersionTupleSyntax.self) else {
        // TODO: Diagnose
        fatalError("expected version after introduced, deprecated, or obsoleted")
      }
      guard let version = VersionTuple(parsing: versionSytnax.trimmedDescription) else {
        // TODO: Diagnose
        fatalError("invalid version string")
      }
      if target != nil {
        // TODO: Diagnose duplicated.
      }

      target = .init(version: version, range: self.generateSourceRange(versionSytnax))
    }

    while let arg = args.popFirst() {
      switch arg.argument {
      case .availabilityVersionRestriction(_):
        fatalError("platformVersion in extended args")

      case .token(let arg):
        // 'deprecated', 'unavailable, 'noasync' changes the mode.
        switch arg.rawText {
        case "deprecated":
          attrKind = .deprecated
        case "unavailable":
          attrKind = .unavailable
        case "noasync":
          attrKind = .noAsync
        default:
          // TODO: Diagnose
          continue
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
          guard let literal = arg.value.as(SimpleStringLiteralExprSyntax.self) else {
            // TODO: Diagnose.
            fatalError("invalid argument type for 'message:'")
          }
          guard let _message = self.generateStringLiteralTextIfNotInterpolated(expr: literal) else {
            fatalError("invalid literal value")
          }
          guard message == nil else {
            fatalError("duplicated 'message' argument")
          }
          message = _message
        case .renamed:
          guard let literal = arg.value.as(SimpleStringLiteralExprSyntax.self) else {
            // TODO: Diagnose.
            fatalError("invalid argument type for 'renamed:'")
          }
          guard let _renamed = self.generateStringLiteralTextIfNotInterpolated(expr: literal) else {
            fatalError("invalid literal value")
          }
          guard renamed == nil else {
            fatalError("duplicated 'message' argument")
          }
          renamed = _renamed
        case .invalid:
          // TODO: Diagnose
          fatalError("invalid labeled argument")
        }
      }
    }

    return BridgedAvailableAttr.createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      range: self.generateAttrSourceRange(node),
      domainString: platformStr.bridged,
      domainLoc: platformLoc,
      kind: attrKind,
      message: message ?? BridgedStringRef(),
      renamed: renamed ?? BridgedStringRef(),
      introduced: introduced?.version.bridged ?? BridgedVersionTuple(),
      introducedRange: introduced?.range ?? BridgedSourceRange(),
      deprecated: deprecated?.version.bridged ?? BridgedVersionTuple(),
      deprecatedRange: deprecated?.range ?? BridgedSourceRange(),
      obsoleted: obsoleted?.version.bridged ?? BridgedVersionTuple(),
      obsoletedRange: obsoleted?.range ?? BridgedSourceRange()
    )
  }

  /// Return true if 'name' is an availability macro name.
  func peekAvailabilityMacroName(_ name: SyntaxText) -> Bool {
    let map = ctx.availabilityMacroMap
    return map.has(name: name.bridged)
  }

  func generate(availabilityMacroDefinition node: AvailabilityMacroDefinitionSyntax) -> BridgedAvailabilityMacroDefinition {

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
    case macro
  }

  func generateAvailabilitySpecList(args node: AvailabilityArgumentListSyntax, context: AvailabilitySpecListContext) -> [BridgedAvailabilitySpec] {
    var result: [BridgedAvailabilitySpec] = []

    func handle(domainNode: TokenSyntax, versionNode: VersionTupleSyntax?) {
      let nameLoc = self.generateSourceLoc(domainNode)
      let version = self.generate(versionTuple: versionNode)
      let versionRange = self.generateSourceRange(versionNode)

      switch domainNode.rawText {
      case "swift", "_PackageVersion":
        let kind: BridgedAvailabilitySpecKind = domainNode.rawText == "swift"
          ? .languageVersionConstraint
          : .packageDescriptionVersionConstraint

        let spec = BridgedPlatformAgnosticVersionConstraintAvailabilitySpec.createParsed(
          self.ctx,
          kind: kind,
          nameLoc: nameLoc,
          version: version?.bridged ?? BridgedVersionTuple(),
          versionRange: versionRange
        )
        result.append(spec.asAvailabilitySpec)

      case let name:
        var macroMatched = false;
        if context != .macro {
          // Try expand macro first.
          let expanded = ctx.availabilityMacroMap.get(
            name: name.bridged,
            version: version?.bridged ?? BridgedVersionTuple()
          )
          if !expanded.isEmpty {
            expanded.withElements(ofType: UnsafeRawPointer.self) { buffer in
              for ptr in buffer {
                result.append(BridgedAvailabilitySpec(raw: UnsafeMutableRawPointer(mutating: ptr)))
              }
            }
            macroMatched = true
          }
        }

        // Was not a macro, it should be a valid platform name.
        if !macroMatched {
          let platform = BridgedPlatformKind(from: name.bridged)
          guard platform != .none else {
            // TODO: Diagnostics.
            fatalError("invalid platform kind")
          }
          guard let version = version else {
            // TODO: Diagnostics.
            fatalError("expected version")
          }
          let spec = BridgedPlatformVersionConstraintAvailabilitySpec.createParsed(
            self.ctx,
            platform: platform,
            platformLoc: nameLoc,
            version: version.bridged,
            runtimeVersion: version.bridged,
            versionRange: versionRange
          )
          result.append(spec.asAvailabilitySpec)
        }
      }
    }

    for parsed in node {
      switch parsed.argument {
      case .token(let tok) where tok.rawText == "*":
        let spec = BridgedOtherPlatformAvailabilitySpec.createParsed(
          self.ctx,
          loc: self.generateSourceLoc(tok)
        )
        result.append(spec.asAvailabilitySpec)
      case .token(let tok):
        handle(domainNode: tok, versionNode: nil)
      case .availabilityVersionRestriction(let platformVersion):
        handle(domainNode: platformVersion.platform, versionNode: platformVersion.version)
      default:
        // TODO: Diagnostics.
        fatalError("invalid argument kind for availability spec")
      }
    }

    return result
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
  let parsed = AvailabilityMacroDefinitionSyntax.parse(from: &parser)

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
