// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/MemberImportVisibility/members_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t -package-name TestPackage %S/Inputs/MemberImportVisibility/members_B.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/MemberImportVisibility/members_C.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %t/A.swift %t/B.swift %t/C.swift -I %t -verify -swift-version 5
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %t/A.swift %t/B.swift %t/C.swift -I %t -verify -swift-version 6
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %t/A.swift %t/B.swift %t/C.swift -I %t -verify -swift-version 5 -enable-upcoming-feature MemberImportVisibility -verify-additional-prefix member-visibility-

// REQUIRES: swift_feature_MemberImportVisibility

//--- main.swift

// expected-member-visibility-note@+2 {{add import of module 'members_A'}}{{1-1=@_implementationOnly import members_A\n}}
// expected-member-visibility-note@+1 {{add import of module 'members_C'}}{{1-1=@_weakLinked @_spiOnly import members_C\n}}
func testMembersWithInferredContextualBase() {
  takesEnumInA(.caseInA) // expected-member-visibility-error{{enum case 'caseInA' is not available due to missing import of defining module 'members_A'}}
  takesEnumInB(.caseInB)
  takesEnumInC(.caseInC) // expected-member-visibility-error{{enum case 'caseInC' is not available due to missing import of defining module 'members_C'}}
}

func testQualifiedMembers() {
  takesEnumInA(EnumInA.caseInA) // expected-error{{cannot find 'EnumInA' in scope; did you mean 'EnumInB'?}}
  takesEnumInB(EnumInB.caseInB)
  takesEnumInC(EnumInC.caseInC) // expected-error{{cannot find 'EnumInC' in scope; did you mean 'EnumInB'?}}
}

//--- A.swift

@_implementationOnly import members_A

func takesEnumInA(_ e: EnumInA) {}

//--- B.swift

@_exported import members_B

func takesEnumInB(_ e: EnumInB) {}

//--- C.swift

@_spiOnly @_weakLinked import members_C

func takesEnumInC(_ e: EnumInC) {}
