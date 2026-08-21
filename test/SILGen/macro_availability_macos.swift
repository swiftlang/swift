// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_BodyMacros
// REQUIRES: swift_feature_CodeItemMacros
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../Macros/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-emit-silgen -swift-version 5 -target %target-cpu-apple-macosx10.50 -enable-experimental-feature BodyMacros -enable-experimental-feature CodeItemMacros -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name main %s > %t/macro_availability.sil

// Each expansion is checked with its own prefix because the order in which
// SILGen emits macro expanded declarations does not match their source order.
// RUN: %FileCheck --check-prefix EXPRESSION --input-file %t/macro_availability.sil %s
// RUN: %FileCheck --check-prefix CODE-ITEM --input-file %t/macro_availability.sil %s
// RUN: %FileCheck --check-prefix DECLARATION --input-file %t/macro_availability.sil %s
// RUN: %FileCheck --check-prefix MEMBER --input-file %t/macro_availability.sil %s
// RUN: %FileCheck --check-prefix PEER --input-file %t/macro_availability.sil %s
// RUN: %FileCheck --check-prefix ACCESSOR --input-file %t/macro_availability.sil %s
// RUN: %FileCheck --check-prefix EXTENSION --input-file %t/macro_availability.sil %s
// RUN: %FileCheck --check-prefix BODY --input-file %t/macro_availability.sil %s
// RUN: %FileCheck --check-prefix MEMBER-ATTRIBUTE --input-file %t/macro_availability.sil %s

@freestanding(expression)
macro expressionThatEmitsCode(_ codeString: String) -> Int =
  #externalMacro(module: "MacroDefinition", type: "ExpressionThatEmitsCodeMacro")

@freestanding(codeItem)
macro codeItemThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "CodeItemThatEmitsCodeMacro")

@freestanding(declaration, names: named(declarationMacroFunc))
macro declarationThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "DeclarationThatEmitsCodeMacro")

@attached(member, names: arbitrary)
macro MemberThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "MemberThatEmitsCodeMacro")

@attached(peer, names: arbitrary)
macro PeerThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "PeerThatEmitsCodeMacro")

@attached(accessor)
macro AccessorThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "AccessorThatEmitsCodeMacro")

@attached(extension, names: arbitrary)
macro ExtensionThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "ExtensionThatEmitsCodeMacro")

@attached(body)
macro BodyThatCallsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "BodyThatCallsCodeMacro")

@attached(memberAttribute)
macro MemberAttributeThatAddsPeer(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "MemberAttributeThatAddsPeerMacro")

// EXPRESSION-LABEL: sil private [ossa] @$s4main19testExpressionMacroSiyFSiyXEfU_ :
// EXPRESSION: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// EXPRESSION: [[MINOR:%.*]] = integer_literal $Builtin.Word, 51
// EXPRESSION: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// EXPRESSION: [[QUERY:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// EXPRESSION: [[RESULT:%.*]] = apply [[QUERY]]([[MAJOR]], [[MINOR]], [[PATCH]])
// EXPRESSION: cond_br [[RESULT]]
func testExpressionMacro() -> Int {
  return #expressionThatEmitsCode("""
    {
      if #available(macOS 10.51, *) {
        return 1
      }
      return 0
    }()
    """)
}

// CODE-ITEM-LABEL: sil hidden [ossa] @$s4main17testCodeItemMacroyyF :
// CODE-ITEM: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CODE-ITEM: [[MINOR:%.*]] = integer_literal $Builtin.Word, 52
// CODE-ITEM: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CODE-ITEM: [[QUERY:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CODE-ITEM: [[RESULT:%.*]] = apply [[QUERY]]([[MAJOR]], [[MINOR]], [[PATCH]])
// CODE-ITEM: cond_br [[RESULT]]
func testCodeItemMacro() {
  #codeItemThatEmitsCode("""
    if #available(macOS 10.52, *) {
    }
    """)
}

// DECLARATION-LABEL: sil hidden [ossa] @$s4main20declarationMacroFuncyyF :
// DECLARATION: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// DECLARATION: [[MINOR:%.*]] = integer_literal $Builtin.Word, 53
// DECLARATION: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// DECLARATION: [[QUERY:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// DECLARATION: [[RESULT:%.*]] = apply [[QUERY]]([[MAJOR]], [[MINOR]], [[PATCH]])
// DECLARATION: cond_br [[RESULT]]
#declarationThatEmitsCode("""
  func declarationMacroFunc() {
    if #available(macOS 10.53, *) {
    }
  }
  """)

// MEMBER-LABEL: sil hidden [ossa] @$s4main15MemberMacroTestV06memberC4FuncyyF :
// MEMBER: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// MEMBER: [[MINOR:%.*]] = integer_literal $Builtin.Word, 54
// MEMBER: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// MEMBER: [[QUERY:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// MEMBER: [[RESULT:%.*]] = apply [[QUERY]]([[MAJOR]], [[MINOR]], [[PATCH]])
// MEMBER: cond_br [[RESULT]]
@MemberThatEmitsCode("""
  func memberMacroFunc() {
    if #available(macOS 10.54, *) {
    }
  }
  """)
struct MemberMacroTest {}

// PEER-LABEL: sil hidden [ossa] @$s4main13PeerMacroTestV04peerC4FuncyyF :
// PEER: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// PEER: [[MINOR:%.*]] = integer_literal $Builtin.Word, 55
// PEER: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// PEER: [[QUERY:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// PEER: [[RESULT:%.*]] = apply [[QUERY]]([[MAJOR]], [[MINOR]], [[PATCH]])
// PEER: cond_br [[RESULT]]
struct PeerMacroTest {
  @PeerThatEmitsCode("""
    func peerMacroFunc() {
      if #available(macOS 10.55, *) {
      }
    }
    """)
  func peerMacroAnchor() {}
}

// ACCESSOR-LABEL: sil hidden [ossa] @$s4main17AccessorMacroTestV5valueSivg :
// ACCESSOR: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// ACCESSOR: [[MINOR:%.*]] = integer_literal $Builtin.Word, 56
// ACCESSOR: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// ACCESSOR: [[QUERY:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// ACCESSOR: [[RESULT:%.*]] = apply [[QUERY]]([[MAJOR]], [[MINOR]], [[PATCH]])
// ACCESSOR: cond_br [[RESULT]]
struct AccessorMacroTest {
  @AccessorThatEmitsCode("""
    get {
      if #available(macOS 10.56, *) {
        return 1
      }
      return 0
    }
    """)
  var value: Int
}

// EXTENSION-LABEL: sil hidden [ossa] @$s4main18ExtensionMacroTestV09extensionC4FuncyyF :
// EXTENSION: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// EXTENSION: [[MINOR:%.*]] = integer_literal $Builtin.Word, 57
// EXTENSION: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// EXTENSION: [[QUERY:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// EXTENSION: [[RESULT:%.*]] = apply [[QUERY]]([[MAJOR]], [[MINOR]], [[PATCH]])
// EXTENSION: cond_br [[RESULT]]
@ExtensionThatEmitsCode("""
  func extensionMacroFunc() {
    if #available(macOS 10.57, *) {
    }
  }
  """)
struct ExtensionMacroTest {}

// BODY-LABEL: sil hidden [ossa] @$s4main13bodyMacroFuncyyF :
// BODY: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// BODY: [[MINOR:%.*]] = integer_literal $Builtin.Word, 58
// BODY: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// BODY: [[QUERY:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// BODY: [[RESULT:%.*]] = apply [[QUERY]]([[MAJOR]], [[MINOR]], [[PATCH]])
// BODY: cond_br [[RESULT]]
@BodyThatCallsCode("""
  if #available(macOS 10.58, *) {
  }
  """)
func bodyMacroFunc() {}

// MEMBER-ATTRIBUTE-LABEL: sil hidden [ossa] @$s4main24MemberAttributeMacroTestV06memberC8PeerFuncyyF :
// MEMBER-ATTRIBUTE: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// MEMBER-ATTRIBUTE: [[MINOR:%.*]] = integer_literal $Builtin.Word, 59
// MEMBER-ATTRIBUTE: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// MEMBER-ATTRIBUTE: [[QUERY:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// MEMBER-ATTRIBUTE: [[RESULT:%.*]] = apply [[QUERY]]([[MAJOR]], [[MINOR]], [[PATCH]])
// MEMBER-ATTRIBUTE: cond_br [[RESULT]]
@MemberAttributeThatAddsPeer("""
  func memberAttributePeerFunc() {
    if #available(macOS 10.59, *) {
    }
  }
  """)
struct MemberAttributeMacroTest {
  func memberAttributeAnchor() {}
}
