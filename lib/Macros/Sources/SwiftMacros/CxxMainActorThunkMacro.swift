//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftDiagnostics
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

/// Macro implementing `@_ExposeMainActorToCxx`.
///
/// Main-actor-isolated declarations cannot be exposed to C++ directly, because
/// C++ callers are not isolated to any actor. When attached to a function,
/// initializer, property, or subscript, this macro introduces a `nonisolated`
/// peer that checks at runtime that it is called on the main actor (using
/// `MainActor.assumeIsolated`) and then forwards to the original declaration.
/// The peer is exposed to C++ under the name of the original declaration, and
/// is unavailable in Swift.
///
/// The same type also implements `@_ExposeMainActorMembersToCxx`, which
/// attaches `@_ExposeMainActorToCxx` to every eligible member of a type or
/// extension.
public enum CxxMainActorThunkMacro {
  static let macroName = "_ExposeMainActorToCxx"
  static let thunkPrefix = "_cxx_"

  /// Wraps the values flowing in and out of the main actor, see
  /// `Forwarding`.
  static let boxType = "_CxxUncheckedSendable"

  /// Attributes copied from the original declaration onto the thunk.
  static let attributesToCopy: Set<String> = [
    "available", "discardableResult", "_lifetime", "lifetime", "_spi",
  ]

  /// Modifiers copied from the original declaration onto the thunk.
  static let modifiersToCopy: Set<String> = [
    "public", "package", "internal", "fileprivate", "private",
    "mutating", "nonmutating", "borrowing", "consuming",
  ]
}

// MARK: - Peer role

extension CxxMainActorThunkMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let cxxName = node.arguments?.as(LabeledExprListSyntax.self)?.first?
      .expression.as(StringLiteralExprSyntax.self)?.representedLiteralValue
    if cxxName != nil,
      !declaration.is(FunctionDeclSyntax.self) && !declaration.is(VariableDeclSyntax.self)
    {
      throw error(node, "a custom C++ name is only supported on functions and properties")
    }
    let container = try Container(context.lexicalContext.first, attribute: node)
    return [try thunk(for: declaration, cxxName: cxxName, container: container)]
  }

  /// The thunk of `declaration`. Throws if it cannot be forwarded.
  ///
  /// Functions, initializers, and subscripts all get a `func` thunk:
  /// initializers and subscripts are first rewritten into the function the
  /// thunk is derived from.
  static func thunk(
    for declaration: some DeclSyntaxProtocol, cxxName: String?, container: Container?
  ) throws -> DeclSyntax {
    var forwarding = Forwarding()
    if let fn = declaration.as(FunctionDeclSyntax.self) {
      // `identifier` strips backticks; operators have none.
      guard let name = fn.name.identifier?.name else {
        throw unsupported(fn.name, "operators")
      }
      try checkSignature(fn.signature)
      let target = forwarding.member(
        fn.name.text, isStatic: hasModifier(fn.modifiers, "static", "class"),
        ownership: hasModifier(fn.modifiers, "mutating")
          ? .mutating : hasModifier(fn.modifiers, "consuming") ? .consuming : .borrowing,
        container: container)
      return forwarding.functionThunk(fn, cxxName: cxxName ?? name, container: container) {
        "\(target)(\($0))"
      }
    }

    // Initializers are forwarded through a static factory function, as the
    // thunk cannot be an initializer with the same signature as the original.
    if let initDecl = declaration.as(InitializerDeclSyntax.self) {
      guard let typeName = container?.typeName else {
        throw error(initDecl, "'@\(macroName)' cannot determine the type of this initializer")
      }
      try checkSignature(initDecl.signature)
      let factory = try FunctionDeclSyntax("""
        \(raw: initDecl.leadingTrivia)\(initDecl.attributes.trimmed) \(initDecl.modifiers.trimmed) \
        static func `init`\(initDecl.genericParameterClause)\(initDecl.signature.trimmed) \
        -> \(raw: typeName)\(initDecl.optionalMark) \(initDecl.genericWhereClause) {}
        """)
      return forwarding.functionThunk(factory, cxxName: "init", container: container) {
        "\(typeName)(\($0))"
      }
    }

    // Subscripts are forwarded through a function exposed as `operator[]`. Like
    // other subscripts exposed to C++, only the getter is available.
    if let subscriptDecl = declaration.as(SubscriptDeclSyntax.self) {
      if hasModifier(subscriptDecl.modifiers, "static", "class") {
        throw unsupported(subscriptDecl, "static subscripts")
      }
      try checkAccessors(subscriptDecl.accessorBlock)
      let receiver = forwarding.receiver(isStatic: false, ownership: .borrowing, container: container)
      // Subscript parameters only have an argument label if they are given two
      // names; spell out the implicit `_` for the function.
      let parameters = subscriptDecl.parameterClause.parameters.map {
        "\($0.secondName == nil ? "_ " : "")\($0.trimmed)"
      }
      let accessor = try FunctionDeclSyntax("""
        \(raw: subscriptDecl.leadingTrivia)\(subscriptDecl.attributes.trimmed) \
        \(subscriptDecl.modifiers.trimmed) \
        func `subscript`\(subscriptDecl.genericParameterClause)(\(raw: parameters.joined())) \
        \(subscriptDecl.returnClause) \(subscriptDecl.genericWhereClause) {}
        """)
      try checkSignature(accessor.signature)
      return forwarding.functionThunk(accessor, cxxName: "operator[]", container: container) {
        "\(receiver ?? "self")[\($0)]"
      }
    }

    if let varDecl = declaration.as(VariableDeclSyntax.self) {
      return try propertyThunk(varDecl, cxxName: cxxName, container: container)
    }
    throw error(
      declaration,
      "'@\(macroName)' can only be attached to functions, initializers, properties, and subscripts")
  }

  static func propertyThunk(
    _ varDecl: VariableDeclSyntax, cxxName: String?, container: Container?
  ) throws -> DeclSyntax {
    guard varDecl.bindings.count == 1, let binding = varDecl.bindings.first,
      let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier,
      let name = identifier.identifier?.name
    else {
      throw error(varDecl, "'@\(macroName)' property must have exactly one named binding")
    }
    guard let type = binding.typeAnnotation?.type.trimmed else {
      throw error(varDecl, "'@\(macroName)' property must have an explicit type annotation")
    }
    try checkAccessors(binding.accessorBlock)

    let accessors = binding.accessorBlock?.accessors.as(AccessorDeclListSyntax.self) ?? []
    func accessor(_ kind: String, has modifier: String) -> Bool {
      accessors.contains { $0.accessorSpecifier.text == kind && hasModifier($0.modifiers, modifier) }
    }
    let isMutatingGet = accessor("get", has: "mutating")
    let isNonmutatingSet = accessor("set", has: "nonmutating")
    if isMutatingGet && isNonmutatingSet {
      throw unsupported(varDecl, "mutating getters with nonmutating setters")
    }
    // Stored properties (including those with observers) are settable if they
    // are variables, computed ones if they have a setter.
    let readers: Set<String> = ["get", "_read", "read", "unsafeAddress", "init"]
    let isSettable = varDecl.bindingSpecifier.tokenKind == .keyword(.var)
      && binding.accessorBlock?.accessors.is(CodeBlockItemListSyntax.self) != true
      && (accessors.isEmpty || accessors.contains { !readers.contains($0.accessorSpecifier.text) })
    let isStatic = hasModifier(varDecl.modifiers, "static", "class")

    var getter = Forwarding()
    let getterCall = getter.member(
      identifier.text, isStatic: isStatic, ownership: isMutatingGet ? .mutating : .borrowing,
      container: container)
    var body = """
      \(isMutatingGet ? "mutating " : "")get {
      \(getter.body(calling: getterCall, throwsClause: nil))
      }
      """
    if isSettable {
      // Setters of value types are mutating by default. Classes are only
      // recognized syntactically, so in extensions of classes `self` cannot be
      // mutated. (PrintAsClang does not expose members of class extensions.)
      let ownership: Forwarding.Ownership =
        isNonmutatingSet || container?.isClass == true ? .borrowing : .mutating
      var setter = Forwarding()
      let property = setter.member(
        identifier.text, isStatic: isStatic, ownership: ownership, container: container)
      let newValue = setter.box("newValue", .borrowing)
      body += """

        \(isNonmutatingSet ? "nonmutating " : "")set {
        \(setter.body(calling: "\(property) = \(newValue)", throwsClause: nil))
        }
        """
    }

    // `private(set)` and friends restrict the setter of the thunk as well.
    return """
      \(raw: header(for: varDecl, cxxName: cxxName ?? name))
      \(raw: modifiers(varDecl.modifiers, container: container)) \
      var \(raw: thunkPrefix + name): \(type) {
      \(raw: body)
      }
      """
  }
}

// MARK: - Forwarding

extension CxxMainActorThunkMacro {
  /// Builds the body of a thunk, which forwards to the original declaration in
  /// a closure isolated to the main actor.
  ///
  /// `MainActor.assumeIsolated` requires its result to be `Sendable`, and
  /// region-based isolation does not allow sending `self` and the parameters
  /// into the closure. So each value flowing in or out of the main actor is
  /// wrapped into an `@unchecked Sendable` box: C++ callers are not checked
  /// for data-race safety, and the thunks are not available in Swift.
  ///
  /// Borrowed and mutated values are boxed as `Ref` and `MutableRef`, so they
  /// are neither copied nor written back, which also supports noncopyable
  /// values.
  struct Forwarding {
    var prologue: [String] = []

    enum Ownership { case borrowing, mutating, consuming }

    /// Boxes `expr` before the call to the original declaration, and returns
    /// the expression accessing the value inside the closure.
    mutating func box(_ expr: String, _ ownership: Ownership) -> String {
      let name = "\(thunkPrefix)box\(prologue.count)"
      switch ownership {
      case .borrowing:
        prologue.append("let \(name) = \(boxType)(Ref(\(expr)))")
        return "\(name).value.value"
      case .mutating:
        prologue.append("var \(name) = \(boxType)(MutableRef(&\(expr)))")
        return "\(name).value.value"
      case .consuming:
        // Taken out of an optional, as a captured value cannot be consumed.
        prologue.append("var \(name) = \(boxType)(Optional(\(expr)))")
        return "\(name).value.take()!"
      }
    }

    /// The expression the original member is accessed on, if any.
    mutating func receiver(
      isStatic: Bool, ownership: Ownership, container: Container?
    ) -> String? {
      guard container != nil else { return nil }
      return isStatic ? "Self" : box("self", ownership)
    }

    /// `name`, qualified with its receiver.
    mutating func member(
      _ name: String, isStatic: Bool, ownership: Ownership, container: Container?
    ) -> String {
      let receiver = receiver(isStatic: isStatic, ownership: ownership, container: container)
      return receiver.map { "\($0).\(name)" } ?? name
    }

    func body(calling call: String, throwsClause: ThrowsClauseSyntax?) -> String {
      let tryKeyword = throwsClause == nil ? "" : "try "
      // The thrown error type of closures is not inferred.
      let signature = throwsClause.map { "() \($0.trimmed) in " } ?? ""
      // `Void` results are boxed too, which keeps the template uniform.
      return (prologue + [
        "return \(tryKeyword)MainActor.assumeIsolated { \(signature)\(tryKeyword)\(boxType)(\(call)) }.take()"
      ]).joined(separator: "\n")
    }

    /// The `func` thunk of `fn`. `call` builds the forwarding call from the
    /// argument list.
    mutating func functionThunk(
      _ fn: FunctionDeclSyntax, cxxName: String, container: Container?,
      call: (String) -> String
    ) -> DeclSyntax {
      let name = fn.name.identifier?.name ?? fn.name.text
      // Unqualified calls (to global functions and initializers) must not be
      // shadowed by parameters.
      let (params, args) = forwardedParameters(
        fn.signature.parameterClause, avoiding: [name, container?.typeName, "self"])
      let throwsClause = fn.signature.effectSpecifiers?.throwsClause
      return """
        \(raw: header(for: fn, cxxName: cxxName))
        \(raw: modifiers(fn.modifiers, container: container)) \
        func \(raw: thunkPrefix + name)\(fn.genericParameterClause?.trimmed)(\(raw: params))\
        \(throwsClause?.trimmed)\(fn.signature.returnClause?.trimmed) \(fn.genericWhereClause?.trimmed) {
        \(raw: body(calling: call(args), throwsClause: throwsClause))
        }
        """
    }

    /// Returns the parameter list of the thunk, and the argument list of the
    /// call to the original declaration. Parameters keep their names, as those
    /// show up in C++, unless they would shadow one of `avoiding`.
    mutating func forwardedParameters(
      _ clause: FunctionParameterClauseSyntax, avoiding: Set<String?>
    ) -> (String, String) {
      var params: [String] = []
      var args: [String] = []
      for (index, param) in clause.parameters.enumerated() {
        let label = param.firstName.identifier?.name
        var internalName = (param.secondName ?? param.firstName).identifier?.name ?? "_"
        if internalName == "_" || avoiding.contains(internalName) {
          internalName = "_\(index)"
        }
        let type = param.type.trimmed
        params.append(
          label == internalName
            ? "`\(internalName)`: \(type)"
            : "\(label.map { "`\($0)`" } ?? "_") `\(internalName)`: \(type)")

        let specifier = type.as(AttributedTypeSyntax.self)?.specifiers.first?
          .as(SimpleTypeSpecifierSyntax.self)?.specifier.text
        let ownership: Ownership =
          specifier == "inout" ? .mutating : specifier == "consuming" ? .consuming : .borrowing
        let value = box("`\(internalName)`", ownership)
        let arg = ownership == .mutating ? "&\(value)" : value
        args.append(label.map { "\($0): \(arg)" } ?? arg)
      }
      return (params.joined(separator: ", "), args.joined(separator: ", "))
    }
  }
}

// MARK: - Helpers

extension CxxMainActorThunkMacro {
  /// The type or extension the declaration the macro is attached to is a
  /// member of. `nil` for global declarations.
  struct Container {
    var isClass: Bool
    var typeName: String?

    init?(_ syntax: Syntax?, attribute: AttributeSyntax) throws {
      guard let syntax else { return nil }
      if syntax.is(ActorDeclSyntax.self) || syntax.is(ProtocolDeclSyntax.self) {
        throw CxxMainActorThunkMacro.unsupported(attribute, "members of actors and protocols")
      }
      isClass = syntax.is(ClassDeclSyntax.self)
      typeName = syntax.as(ExtensionDeclSyntax.self)?.extendedType.trimmedDescription
        ?? syntax.asProtocol(NamedDeclSyntax.self)?.name.text
    }
  }

  /// The attributes, documentation, and C++ name of the thunk.
  static func header(for decl: some WithAttributesSyntax, cxxName: String) -> String {
    var lines = decl.leadingTrivia.compactMap { piece -> String? in
      switch piece {
      case .docLineComment(let text), .docBlockComment(let text): text
      default: nil
      }
    }
    for case .attribute(let attribute) in decl.attributes
    where attributesToCopy.contains(attributeName(attribute)) {
      lines.append(attribute.trimmedDescription)
    }
    lines.append("@available(swift, obsoleted: 1, message: \"only available in C++\")")
    // `Ref` and `MutableRef` need the Swift 6.4 runtime.
    lines.append("@available(macOS 27, iOS 27, tvOS 27, watchOS 27, visionOS 27, *)")
    lines.append("@_expose(Cxx, \"\(cxxName)\")")
    return lines.joined(separator: "\n")
  }

  /// The modifiers of the thunk: the access level and kind of the original
  /// declaration, made `nonisolated`.
  static func modifiers(_ modifiers: DeclModifierListSyntax, container: Container?) -> String {
    var result = modifiers.compactMap { modifier -> String? in
      switch modifier.name.text {
      case "open": "public"
      case let name where modifiersToCopy.contains(name): modifier.trimmedDescription
      default: nil
      }
    }
    if hasModifier(modifiers, "static", "class") {
      result.append("static")
    } else if container?.isClass == true {
      result.append("final")
    }
    return (result + ["nonisolated"]).joined(separator: " ")
  }

  /// The last component of the name of an attribute, e.g. `available` or
  /// `_ExposeMainActorToCxx` for `@Cxx._ExposeMainActorToCxx`.
  static func attributeName(_ attribute: AttributeSyntax) -> String {
    attribute.attributeName.as(MemberTypeSyntax.self)?.name.text
      ?? attribute.attributeName.trimmedDescription
  }

  static func hasModifier(_ modifiers: DeclModifierListSyntax, _ names: String...) -> Bool {
    modifiers.contains { names.contains($0.name.text) }
  }

  // MARK: Unsupported declarations

  static func error(_ syntax: some SyntaxProtocol, _ message: String) -> DiagnosticsError {
    DiagnosticsError(syntax: syntax, message: message, id: .incompatibleDecl)
  }

  static func unsupported(_ syntax: some SyntaxProtocol, _ what: String) -> DiagnosticsError {
    error(syntax, "'@\(macroName)' does not support \(what)")
  }

  /// Diagnoses parameters and effects the thunk cannot forward.
  static func checkSignature(_ signature: FunctionSignatureSyntax) throws {
    if let effects = signature.effectSpecifiers {
      if effects.asyncSpecifier != nil {
        throw unsupported(effects, "async declarations")
      }
      if effects.throwsClause?.throwsSpecifier.tokenKind == .keyword(.rethrows) {
        throw unsupported(effects, "rethrows")
      }
    }
    for param in signature.parameterClause.parameters {
      if param.ellipsis != nil {
        throw unsupported(param, "variadic parameters")
      }
      if let attributed = param.type.as(AttributedTypeSyntax.self),
        attributed.attributes.contains(where: {
          $0.as(AttributeSyntax.self).map(attributeName) == "autoclosure"
        })
      {
        throw unsupported(param, "autoclosure parameters")
      }
    }
  }

  /// Diagnoses effectful accessors, which C++ cannot represent.
  static func checkAccessors(_ block: AccessorBlockSyntax?) throws {
    for accessor in block?.accessors.as(AccessorDeclListSyntax.self) ?? [] {
      if accessor.effectSpecifiers?.asyncSpecifier != nil {
        throw unsupported(accessor, "async accessors")
      }
      if accessor.effectSpecifiers?.throwsClause != nil {
        throw unsupported(accessor, "throwing accessors")
      }
    }
  }
}

// MARK: - Member attribute role

extension CxxMainActorThunkMacro: MemberAttributeMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: some DeclGroupSyntax,
    providingAttributesFor member: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [AttributeSyntax] {
    let container = try Container(Syntax(declaration), attribute: node)
    guard isEligibleMember(member, container: container) else { return [] }
    return ["@\(raw: macroName)"]
  }

  /// Whether the member should get a thunk when the macro is attached to the
  /// enclosing type or extension. Members that the thunk could not forward, or
  /// that C++ could not represent, are skipped rather than diagnosed.
  static func isEligibleMember(_ member: some DeclSyntaxProtocol, container: Container?) -> Bool {
    guard let decl = member.asProtocol(WithModifiersSyntax.self),
      let attributed = member.asProtocol(WithAttributesSyntax.self),
      // Only members visible to C++ are interesting; `nonisolated` members can
      // be exposed directly, and overrides are reached through the thunk of
      // the overridden declaration.
      !hasModifier(decl.modifiers, "private", "fileprivate", "nonisolated", "override"),
      // Do not attach the macro twice.
      !attributed.attributes.contains(where: {
        $0.as(AttributeSyntax.self).map(attributeName) == macroName
      }),
      // Skip members the peer macro would diagnose.
      let thunk = try? thunk(for: member, cxxName: nil, container: container)
    else {
      return false
    }
    // C++ cannot represent throwing functions or generic requirements.
    guard let fn = thunk.as(FunctionDeclSyntax.self) else { return true }
    return fn.signature.effectSpecifiers?.throwsClause == nil && fn.genericWhereClause == nil
      && fn.genericParameterClause?.parameters.allSatisfy { $0.inheritedType == nil } ?? true
  }
}
