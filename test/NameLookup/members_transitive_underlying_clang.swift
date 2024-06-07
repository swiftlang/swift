// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -primary-file %t/Primary.swift %t/Other.swift -I %S/Inputs/MemberImportVisibility -module-name Underlying -verify -swift-version 5
// RUN: %target-swift-frontend -typecheck -primary-file %t/Primary.swift %t/Other.swift -I %S/Inputs/MemberImportVisibility -module-name Underlying -verify -swift-version 6
// RUN: %target-swift-frontend -typecheck -primary-file %t/Primary.swift %t/Other.swift -I %S/Inputs/MemberImportVisibility -module-name Underlying -verify -swift-version 5 -enable-experimental-feature MemberImportVisibility -verify-additional-prefix member-visibility-

//--- Other.swift
@_exported import Underlying

//--- Primary.swift

// expected-member-visibility-note@+1 {{add import of module 'Underlying'}}{{1-1=@_exported import Underlying\n}}
func test(_ s: UnderlyingStruct) {
  _ = s.a // expected-member-visibility-error {{property 'a' is not available due to missing import of defining module 'Underlying'}}
}
