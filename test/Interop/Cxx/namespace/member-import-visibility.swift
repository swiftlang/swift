// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop
// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop -enable-upcoming-feature MemberImportVisibility -verify-additional-prefix member-visibility-

// REQUIRES: swift_feature_MemberImportVisibility

import MembersDirect

// expected-member-visibility-note@-1 4 {{add import of module 'MembersTransitive'}}

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

func test() {
  let _: Bool = returnsTransitiveStruct().memberVar // expected-member-visibility-error {{property 'memberVar' is not available due to missing import of defining module 'MembersTransitive'}}
  let _: Bool = returnsTopLevelTransitiveStruct().memberVar // expected-member-visibility-error {{property 'memberVar' is not available due to missing import of defining module 'MembersTransitive'}}
}
