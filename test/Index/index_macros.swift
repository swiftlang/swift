// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// Check that we index code expanded from macros, especially nested references
// (ie. calls within an added function).

// Create the plugin with various macros for testing
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(IndexMacros) -module-name=IndexMacros %t/IndexMacros.swift -g -no-toolchain-stdlib-rpath

// Check indexed symbols
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %t/IndexTest.swift -load-plugin-library %t/%target-library-name(IndexMacros) -parse-as-library 2>&1 | tee %t/test.idx | %FileCheck %s

//--- IndexTest.swift
@freestanding(expression)
macro freestandingExpr() = #externalMacro(module: "IndexMacros", type: "FreestandingExprMacro")
// CHECK: [[@LINE-1]]:7 | macro/Swift | freestandingExpr() |  [[EXPR_USR:.*]] | Def

@freestanding(declaration, names: named(TestFree))
macro freestandingDecl() = #externalMacro(module: "IndexMacros", type: "FreestandingDeclMacro")
// CHECK: [[@LINE-1]]:7 | macro/Swift | freestandingDecl() |  [[DECL_USR:.*]] | Def

@attached(accessor)
macro Accessor() = #externalMacro(module: "IndexMacros", type: "SomeAccessorMacro")

@attached(conformance)
@attached(member, names: named(attachedAddedMember))
@attached(memberAttribute)
@attached(peer, names: named(TestPeer))
macro AllAttached() = #externalMacro(module: "IndexMacros", type: "AllAttachedMacro")
// CHECK: [[@LINE-1]]:7 | macro/Swift | AllAttached() |  [[ATTACHED_USR:.*]] | Def

protocol TestProto {}
// CHECK: [[@LINE-1]]:10 | protocol/Swift | TestProto | [[PROTO_USR:.*]] | Def

func accessorLog() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | accessorLog() | [[ACC_LOG_USR:.*]] | Def
func exprLog() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | exprLog() | [[EXPR_LOG_USR:.*]] | Def
func freeLog() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | freeLog() | [[FREE_LOG_USR:.*]] | Def
func memberLog() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | memberLog() | [[MEMBER_LOG_USR:.*]] | Def
func peerLog() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | peerLog() | [[PEER_LOG_USR:.*]] | Def

// CHECK: [[@LINE+2]]:8 | struct/Swift | AddOne | [[ADD_ONE_USR:.*]] | Def
@propertyWrapper
struct AddOne {
  var value: Int = 1
  var wrappedValue: Int {
    get { value }
    set { value = newValue + 1 }
  }
  init(wrappedValue: Int) {
    self.wrappedValue = wrappedValue
  }
}

// Creates a `TestFree` struct with `freeFunc` calling `freeLog`
#freestandingDecl
// CHECK: [[@LINE-1]]:2 | macro/Swift | freestandingDecl() | [[DECL_USR]] | Ref
// CHECK: [[@LINE-2]]:1 | struct/Swift | TestFree | [[FREE_STRUCT_USR:.*]] | Def,Impl
// CHECK: [[@LINE-3]]:1 | instance-method/Swift | freeFunc() | [[FREE_FUNC_USR:.*]] | Def,Impl,RelChild
// CHECK-NEXT: RelChild | struct/Swift | TestFree | [[FREE_STRUCT_USR]]
// CHECK: [[@LINE-5]]:1 | function/Swift | freeLog() | [[FREE_LOG_USR]] | Ref,Call,Impl,RelCall,RelCont
// CHECK-NEXT: RelCall,RelCont | instance-method/Swift | freeFunc() | [[FREE_FUNC_USR]]

// CHECK: [[@LINE+1]]:2 | macro/Swift | AllAttached() | [[ATTACHED_USR]] | Ref
@AllAttached
struct TestAttached {
  var attachedMember: Int

  @Accessor
  var attachedMemberAccessors: Int
}
// `@AddOne` is added to attachedMember
// CHECK: [[@LINE-8]]:1 | struct/Swift | AddOne | [[ADD_ONE_USR]] | Ref,Impl,RelCont
// CHECK-NEXT: RelCont | instance-property/Swift | attachedMember

// `accessorLog` is referenced in the added getter/setter on `attachedMemberAccessors`
// CHECK: [[@LINE-8]]:3 | function/Swift | accessorLog() | [[ACC_LOG_USR]] | Ref,Call,Impl,RelCall,RelCont
// CHECK-NEXT: RelCall,RelCont | instance-method/acc-get/Swift | getter:attachedMemberAccessors

// `memberFunc` is added to `TestAttached` and contains a call to `memberLog`
// CHECK: [[@LINE-16]]:1 | instance-method/Swift | memberFunc() | [[MEMBER_USR:.*]] | Def,Impl
// CHECK: [[@LINE-17]]:1 | function/Swift | memberLog() | [[MEMBER_LOG_USR]] | Ref,Call,Impl,RelCall,RelCont
// CHECK-NEXT: RelCall,RelCont | instance-method/Swift | memberFunc() | [[MEMBER_USR]]

// `TestPeer` is added as a peer to `TestAttached` and contains `peerFunc` with a call to `peerLog`
// CHECK: [[@LINE-21]]:1 | struct/Swift | TestPeer | [[PEER_STRUCT_USR:.*]] | Def,Impl
// CHECK: [[@LINE-22]]:1 | instance-method/Swift | peerFunc() | [[PEER_FUNC_USR:.*]] | Def,Impl,RelChild
// CHECK-NEXT: RelChild | struct/Swift | TestPeer | [[PEER_STRUCT_USR]]
// CHECK: [[@LINE-24]]:1 | function/Swift | peerLog() | [[PEER_LOG_USR]] | Ref,Call,Impl,RelCall,RelCont
// CHECK-NEXT: RelCall,RelCont | instance-method/Swift | peerFunc() | [[PEER_FUNC_USR]]

// `TestProto` is added as a conformance to `TestAttached`
// CHECK: [[@LINE-28]]:1 | extension/ext-struct/Swift | TestAttached | [[EXT_USR:.*]] | Def,Impl
// CHECK: [[@LINE-29]]:1 | protocol/Swift | TestProto | [[PROTO_USR]] | Ref,Impl,RelBase

func testExpr() {
  #freestandingExpr
}

//--- IndexMacros.swift
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct FreestandingExprMacro: ExpressionMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> ExprSyntax {
    return "exprLog()"
  }
}

public struct FreestandingDeclMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return ["""
      struct TestFree {
        func freeFunc() {
          freeLog()
        }
      }
      """]
  }
}


public struct SomeAccessorMacro: AccessorMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingAccessorsOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [AccessorDeclSyntax] {
    return [
      """
        get {
          accessorLog()
          return 1
        }
      """,
      """
        set {
          accessorLog()
        }
      """,
    ]
  }
}

public struct AllAttachedMacro {}

extension AllAttachedMacro: ConformanceMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingConformancesOf decl: some DeclGroupSyntax,
    in context: some MacroExpansionContext
  ) throws -> [(TypeSyntax, GenericWhereClauseSyntax?)] {
    let protocolName: TypeSyntax = "TestProto"
    return [(protocolName, nil)]
  }
}

extension AllAttachedMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let newFunc: DeclSyntax =
      """
      func memberFunc() {
        memberLog()
      }
      """
    return [
      newFunc,
    ]
  }
}

extension AllAttachedMacro: MemberAttributeMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo parent: some DeclGroupSyntax,
    providingAttributesFor member: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [AttributeSyntax] {
    guard let varDecl = member.as(VariableDeclSyntax.self),
      let binding = varDecl.bindings.first,
      let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier.text,
      identifier == "attachedMember"
    else {
      return []
    }

    return [AttributeSyntax(
      attributeName: SimpleTypeIdentifierSyntax(
        name: .identifier("AddOne")
      )
    )]
  }
}

extension AllAttachedMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return [
      """
      struct TestPeer {
        func peerFunc() {
          peerLog()
        }
      }
      """
    ]
  }
}
