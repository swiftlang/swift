// Import-access tracking for macro-expanded code: references made by an
// expansion are attributed to the user file holding the import (PR #86376).

// REQUIRES: swift_swift_parser
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_InternalImportsByDefault

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// Build the shared macro plugin.
// RUN: %host-build-swift -swift-version 5 -emit-library \
// RUN:   -o %t/%target-library-name(MacroDefinition) \
// RUN:   -module-name=MacroDefinition \
// RUN:   %S/Inputs/syntax_macro_definitions.swift \
// RUN:   -g -no-toolchain-stdlib-rpath

// Build the two test libraries.
// RUN: %target-swift-frontend -emit-module -module-name MacroLib \
// RUN:   -o %t/MacroLib.swiftmodule %t/MacroLib.swift \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module -module-name OtherLib \
// RUN:   -o %t/OtherLib.swiftmodule %t/OtherLib.swift \
// RUN:   -enable-library-evolution

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_PeerWithHelper.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_PeerNoTypeRef.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_PeerMultiModule.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_Member.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_Extension.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_Declaration.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_PublicSymmetry.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_PublicNoTypeRef.swift

// The access-level error originates inside the expansion buffer, matched by
// an expansion block; -verify-ignore-unrelated drops MacroLib interface noise.
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_InternalIsTooLow.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_PublicImport_PackageOnlyExpansion.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_Accessor.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_Expression.swift

// RUN: %target-swift-frontend -typecheck -verify -package-name pkg \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   -I %t %t/Client_MemberAttribute.swift

//--- MacroLib.swift
public struct Helper {
  public init() {}
}

// Generic emitters: each re-emits its string-literal argument verbatim as code.
@attached(peer, names: suffixed(Mock))
public macro PeerThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "PeerThatEmitsCodeMacro")

@attached(member, names: arbitrary)
public macro MemberThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "MemberThatEmitsCodeMacro")

@attached(extension, names: arbitrary)
public macro ExtensionThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "ExtensionThatEmitsCodeMacro")

@freestanding(declaration, names: arbitrary)
public macro declarationThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "DeclarationThatEmitsCodeMacro")

@attached(accessor)
public macro AccessorThatEmitsCode(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "AccessorThatEmitsCodeMacro")

@freestanding(expression)
public macro expressionThatEmitsCode(_ codeString: String) -> Helper =
  #externalMacro(module: "MacroDefinition", type: "ExpressionThatEmitsCodeMacro")

@attached(memberAttribute)
public macro MemberAttributeThatAddsPeer(_ codeString: String) =
  #externalMacro(module: "MacroDefinition", type: "MemberAttributeThatAddsPeerMacro")

//--- OtherLib.swift
public struct OtherHelper {
  public init() {}
}

//--- Client_PeerWithHelper.swift
// Peer expansion references Helper at package level, so the import is used.
package import MacroLib // expected-no-diagnostics

@PeerThatEmitsCode("package final class PeerWithHelperServiceMock { package var helper: Helper = Helper() }")
package protocol PeerWithHelperService {
  func doThing()
}

//--- Client_PeerNoTypeRef.swift
// Expansion references only Int, so package import is genuinely unused: warn.
package import MacroLib // expected-warning {{package import of 'MacroLib' was not used in package declarations}}

@PeerThatEmitsCode("package final class PeerNoTypeRefServiceMock { package var counter: Int = 0 }")
package protocol PeerNoTypeRefService {
  func doThing()
}

//--- Client_PeerMultiModule.swift
// Expansion references OtherHelper (OtherLib) at package level; MacroLib itself is unused.
package import MacroLib // expected-warning {{package import of 'MacroLib' was not used in package declarations}}
package import OtherLib // expected-no-diagnostics

@PeerThatEmitsCode("package final class PeerMultiModuleServiceMock { package var helper: OtherHelper = OtherHelper() }")
package protocol PeerMultiModuleService {
  func doThing()
}

//--- Client_Member.swift
// Member-macro expansion adds a package member typed from MacroLib.
package import MacroLib // expected-no-diagnostics

@MemberThatEmitsCode("package var helper: Helper = Helper()")
package class MemberService {
  package init() {}
}

//--- Client_Extension.swift
// Extension-macro expansion adds a package method returning a MacroLib type.
package import MacroLib // expected-no-diagnostics

@ExtensionThatEmitsCode("package func makeHelper() -> Helper { Helper() }")
package class ExtensionService {}

//--- Client_Declaration.swift
// Freestanding declaration macro emits a package class with a MacroLib-typed property.
package import MacroLib // expected-no-diagnostics

#declarationThatEmitsCode("package final class FreestandingMock { package var helper: Helper = Helper() }")

//--- Client_PublicSymmetry.swift
// Public analogue of Client_PeerWithHelper; must not regress.
public import MacroLib // expected-no-diagnostics

@PeerThatEmitsCode("public final class PublicSymmetryServiceMock { public var helper: Helper = Helper() }")
public protocol PublicSymmetryService {
  func doThing()
}

//--- Client_PublicNoTypeRef.swift
// Public analogue of Client_PeerNoTypeRef; only this case proves the MacroDecl skip on the public path.
public import MacroLib // expected-warning {{public import of 'MacroLib' was not used in public declarations}}

@PeerThatEmitsCode("package final class PublicNoTypeRefServiceMock { package var counter: Int = 0 }")
public protocol PublicNoTypeRefService {
  func doThing()
}

//--- Client_InternalIsTooLow.swift
// Negative guard: package expansion needs Helper but import is internal, so the access-level error must still fire.
internal import MacroLib // expected-note {{struct 'Helper' imported as 'internal' from 'MacroLib' here}}

// expected-note@+1 2 {{in expansion of macro 'PeerThatEmitsCode' on protocol 'InternalIsTooLowService' here}}
@PeerThatEmitsCode("""
package final class InternalIsTooLowServiceMock {
  package var helper: Helper = Helper()
}
""")
package protocol InternalIsTooLowService {
  func doThing()
}
/*
expected-expansion@-2:2 {{
  expected-error@2:15{{property cannot be declared package because its type uses an internal type}}
  expected-note@2:15{{struct 'Helper' is imported by this file as 'internal' from 'MacroLib'}}
}}
*/

//--- Client_PublicImport_PackageOnlyExpansion.swift
// Expansion references Helper only at package level, so the public import is overkill: warn.
public import MacroLib // expected-warning {{public import of 'MacroLib' was not used in public declarations}}

@PeerThatEmitsCode("package final class PublicImportPackageOnlyServiceMock { package var helper: Helper = Helper() }")
package protocol PublicImportPackageOnlyService {
  func doThing()
}

//--- Client_Accessor.swift
// Accessor body references Helper (not API surface), so the package import is oversized: warn.
package import MacroLib // expected-warning {{package import of 'MacroLib' was not used in package declarations}}

package class AccessorService {
  @AccessorThatEmitsCode("get { _ = Helper(); return 42 }")
  package var value: Int
}

//--- Client_Expression.swift
// Expression macro in a function body; same body-context reasoning as Client_Accessor.
package import MacroLib // expected-warning {{package import of 'MacroLib' was not used in package declarations}}

package func consumeExpression() {
  _ = #expressionThatEmitsCode("Helper()")
}

//--- Client_MemberAttribute.swift
// Member-attribute macro adds a peer macro, so the walk-up must reach this file through both buffers.
package import MacroLib // expected-no-diagnostics

@MemberAttributeThatAddsPeer("package var doThingMock: Helper = Helper()")
package class MemberAttributeService {
  package func doThing() {}
}
