// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop %S/Inputs/imports-members-unrelated.swift
// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop -enable-upcoming-feature MemberImportVisibility -verify-additional-prefix member-visibility- %S/Inputs/imports-members-unrelated.swift

// REQUIRES: swift_feature_MemberImportVisibility

import MembersDirect

// expected-member-visibility-note@-1 4 {{add import of module 'MembersTransitive'}}
// expected-member-visibility-note@-2 2 {{add import of module 'MembersUnrelated'}}

extension DirectNamespace { }
extension TransitiveNamespace { } // expected-error {{cannot find type 'TransitiveNamespace' in scope}}

extension CommonNamespace.DirectStruct {
  func extensionMethod() -> Int32 {
    return memberVar
  }
}

extension CommonNamespace.TransitiveStruct { // expected-member-visibility-error {{struct 'TransitiveStruct' is not available due to missing import of defining module 'MembersTransitive'}}
  func extensionMethod() -> Bool {
    return memberVar // expected-member-visibility-error {{property 'memberVar' is not available due to missing import of defining module 'MembersTransitive'}}
  }
}

// Another file imports MembersUnrelated, which also declares
// NestedCommonNamespace. That doesn't hide the namespace from this file.
extension CommonNamespace.NestedCommonNamespace.NestedDirectStruct { }
extension CommonNamespace.MembersDirect::NestedCommonNamespace.NestedDirectStruct { }
extension CommonNamespace.NestedCommonNamespace.NestedUnrelatedStruct { } // expected-member-visibility-error {{struct 'NestedUnrelatedStruct' is not available due to missing import of defining module 'MembersUnrelated'}}
extension CommonNamespace.NestedUnrelatedNamespace { } // expected-member-visibility-error {{enum 'NestedUnrelatedNamespace' is not available due to missing import of defining module 'MembersUnrelated'}}

func test() {
  let _: Bool = returnsTransitiveStruct().memberVar // expected-member-visibility-error {{property 'memberVar' is not available due to missing import of defining module 'MembersTransitive'}}
  let _: Bool = returnsTopLevelTransitiveStruct().memberVar // expected-member-visibility-error {{property 'memberVar' is not available due to missing import of defining module 'MembersTransitive'}}
}
