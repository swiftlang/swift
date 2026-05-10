// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/MemberImportVisibility/members_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t -package-name TestPackage %S/Inputs/MemberImportVisibility/members_B.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/MemberImportVisibility/members_C.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %t/A.swift %t/B.swift %t/C.swift -I %t -verify -verify-ignore-unrelated -swift-version 5
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %t/A.swift %t/B.swift %t/C.swift -I %t -verify -verify-ignore-unrelated -swift-version 6
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %t/A.swift %t/B.swift %t/C.swift -I %t -verify -verify-ignore-unrelated -swift-version 5 -enable-upcoming-feature MemberImportVisibility -verify-additional-prefix member-visibility-

// REQUIRES: swift_feature_MemberImportVisibility

//--- main.swift

// expected-member-visibility-note@+2 1 {{add import of module 'members_A'}}{{1-1=@_implementationOnly import members_A\n}}
// expected-member-visibility-note@+1 1 {{add import of module 'members_C'}}{{1-1=@_weakLinked @_spiOnly import members_C\n}}
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

// FIXME: Should diagnose import needed for defaultedRequirementInC().
struct ConformsToRefinesProtocolInA1: RefinesProtocolInA1 {}
// expected-member-visibility-error@-1 {{type 'ConformsToRefinesProtocolInA1' does not conform to protocol 'ProtocolInA1'}}
// expected-member-visibility-note@-2 {{add stubs for conformance}}

// FIXME: Should diagnose import needed for defaultedRequirementInA()/defaultedRequirementInC().
struct ConformsToRefinesProtocolInA2: RefinesProtocolInA2 {}
// expected-member-visibility-error@-1 {{type 'ConformsToRefinesProtocolInA2' does not conform to protocol 'ProtocolInA2'}}
// expected-member-visibility-note@-2 {{add stubs for conformance}}

struct ConformsToRefinesProtocolInA3: RefinesProtocolInA3 {}
struct ConformsToRefinesProtocolInC1: RefinesProtocolInC1 {}

// FIXME: Should diagnose import needed for defaultedRequirementInC().
struct ConformsToRefinesProtocolInC2: RefinesProtocolInC2 {}
// expected-member-visibility-error@-1 {{type 'ConformsToRefinesProtocolInC2' does not conform to protocol 'ProtocolInB2'}}
// expected-member-visibility-note@-2 {{add stubs for conformance}}

//--- A.swift

@_implementationOnly import members_A

func takesEnumInA(_ e: EnumInA) {}

protocol RefinesProtocolInA1: ProtocolInA1 {}
protocol RefinesProtocolInA2: ProtocolInA2 {}
protocol RefinesProtocolInA3: ProtocolInA3 {}

//--- B.swift

@_exported import members_B

func takesEnumInB(_ e: EnumInB) {}

//--- C.swift

@_spiOnly @_weakLinked import members_C

func takesEnumInC(_ e: EnumInC) {}

protocol RefinesProtocolInC1: ProtocolInC1 {}
protocol RefinesProtocolInC2: ProtocolInC2 {}
