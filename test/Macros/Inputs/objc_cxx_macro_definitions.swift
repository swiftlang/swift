import SwiftParser
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

struct ObjCMemberFuncMacro: MemberMacro {
  static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax], 
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let decl = declaration.asProtocol(NamedDeclSyntax.self) else {
      return []
    }
    return ["@objc public func member_\(raw: decl.name.text)() {}"]
  }
}

struct ObjCPeerFuncMacro: PeerMacro {
  static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let decl = declaration.asProtocol(NamedDeclSyntax.self) else {
      return []
    }
    return ["@objc public func peer_\(raw: decl.name.text)() {}"]
  }
}

struct ObjCFreestandingFuncMacro: DeclarationMacro {
  static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return ["@objc public func member_freestanding() {}"]
  }
}

struct ObjCFreestandingClassMacro: DeclarationMacro {
  static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return ["""
      @objc public class MacroExpandedObjCClass: NSObject {
        @objc public func member() {}
      }
      """]
  }
}

struct CDeclFreestandingFuncMacro: DeclarationMacro {
  static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return [#"@_cdecl("c_freestanding") public func cFreestanding() {}"#]
  }
}

struct ObjCExtensionMacro: ExtensionMacro {
  static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: some DeclGroupSyntax,
    providingExtensionsOf type: some TypeSyntaxProtocol,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [ExtensionDeclSyntax] {
    let decl: DeclSyntax = """
      extension \(type): MyObjCProtocol {
        public func objcRequirement() {}
      }
      """
    return [decl.as(ExtensionDeclSyntax.self)!]
  }
}

struct MemberFuncMacro: MemberMacro {
  static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax], 
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let decl = declaration.asProtocol(NamedDeclSyntax.self) else {
      return []
    }
    return ["public func member_\(raw: decl.name.text)() {}"]
  }
}

struct PeerFuncMacro: PeerMacro {
  static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let decl = declaration.asProtocol(NamedDeclSyntax.self) else {
      return []
    }
    return ["public func peer_\(raw: decl.name.text)() {}"]
  }
}

struct CxxFreestandingFuncMacro: DeclarationMacro {
  static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return ["public func cxxFreestanding() {}"]
  }
}

struct CxxFreestandingStructMacro: DeclarationMacro {
  static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return ["""
      public struct MacroExpandedStruct {
        private let x: Int = 0
        public func member() {}
      }
      """]
  }
}
