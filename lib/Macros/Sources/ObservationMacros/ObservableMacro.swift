import SwiftSyntax
import SwiftSyntaxMacros

@_implementationOnly import SwiftDiagnostics
@_implementationOnly import SwiftOperators
@_implementationOnly import SwiftSyntaxBuilder

private extension DeclSyntaxProtocol {
  var isObservableStoredProperty: Bool {
    guard let property = self.as(VariableDeclSyntax.self),
          let binding = property.bindings.first
    else {
      return false
    }

    return binding.accessor == nil
  }
}

public struct ObservableMacro: MemberMacro, MemberAttributeMacro, ConformanceMacro {
  // MARK: - ConformanceMacro
  public static func expansion<
    Declaration: DeclGroupSyntax,
    Context: MacroExpansionContext
  >(
    of node: AttributeSyntax,
    providingConformancesOf declaration: Declaration,
    in context: Context
  ) throws -> [(TypeSyntax, GenericWhereClauseSyntax?)] {
    let protocolName: TypeSyntax = "Observable"
    return [(protocolName, nil)]
  }

  // MARK: - MemberMacro
  public static func expansion<
    Declaration: DeclGroupSyntax,
    Context: MacroExpansionContext
  >(
    of node: AttributeSyntax,
    providingMembersOf declaration: Declaration,
    in context: Context
  ) throws -> [DeclSyntax] {
    guard let identified = declaration.asProtocol(IdentifiedDeclSyntax.self) else {
      return []
    }

    let parentName = identified.identifier

    let registrar: DeclSyntax = 
      """
      let _registrar = ObservationRegistrar<\(parentName)>()
      """

    let transactions: DeclSyntax =
      """
      public nonisolated func transactions<Delivery>(for keyPaths: KeyPaths<\(parentName)>, isolation: Delivery) -> ObservedTransactions<\(parentName), Delivery> where Delivery: Actor {
        _registrar.transactions(for: keyPaths, isolation: isolation)
      }
      """

    let changes: DeclSyntax =
      """
      public nonisolated func changes<Member>(for keyPath: KeyPath<\(parentName), Member>) -> ObservedChanges<\(parentName), Member> where Member: Sendable {
        _registrar.changes(for: keyPath)
      }
      """

    let memberList = MemberDeclListSyntax(
      declaration.members.members.filter {
        $0.decl.isObservableStoredProperty
      }
    )

    let storageStruct: DeclSyntax =
      """
      private struct _Storage {
      \(memberList)
      }
      """

    let storage: DeclSyntax =
      """
      private var _storage = _Storage()
      """

    return [
      registrar,
      transactions,
      changes,
      storageStruct,
      storage,
    ]
  }

  // MARK: - MemberAttributeMacro

  public static func expansion<
    Declaration: DeclGroupSyntax,
    MemberDeclaration: DeclSyntaxProtocol,
    Context: MacroExpansionContext
  >(
    of node: AttributeSyntax,
    attachedTo declaration: Declaration,
    providingAttributesFor member: MemberDeclaration,
    in context: Context
  ) throws -> [AttributeSyntax] {
    guard member.isObservableStoredProperty else {
      return []
    }

    return [
      AttributeSyntax(
        attributeName: SimpleTypeIdentifierSyntax(
          name: .identifier("ObservableProperty")
        )
      )
    ]
  }
}

public struct ObservablePropertyMacro: AccessorMacro {
  public static func expansion<
    Context: MacroExpansionContext,
    Declaration: DeclSyntaxProtocol
  >(
    of node: AttributeSyntax,
    providingAccessorsOf declaration: Declaration,
    in context: Context
  ) throws -> [AccessorDeclSyntax] {
    guard let property = declaration.as(VariableDeclSyntax.self),
      let binding = property.bindings.first,
      let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier,
      binding.accessor == nil
    else {
      return []
    }

    if identifier.text == "_registrar" || identifier.text == "_storage" { return [] }

    let getAccessor: AccessorDeclSyntax =
      """
      get {
        _registrar.access(self, keyPath: \\.\(identifier))
        return _storage.\(identifier)
      }
      """

    let setAccessor: AccessorDeclSyntax =
      """
      set {
        _registrar.withMutation(of: self, keyPath: \\.\(identifier)) {
          _storage.\(identifier) = newValue
        }
      }
      """

    return [getAccessor, setAccessor]
  }
}
