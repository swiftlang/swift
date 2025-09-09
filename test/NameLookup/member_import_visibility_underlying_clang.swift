// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -primary-file %t/Primary.swift %t/Other.swift -I %S/Inputs/MemberImportVisibility -module-name Underlying -verify -swift-version 5
// RUN: %target-swift-frontend -typecheck -primary-file %t/Primary.swift %t/Other.swift -I %S/Inputs/MemberImportVisibility -module-name Underlying -verify -swift-version 6
// RUN: %target-swift-frontend -typecheck -primary-file %t/Primary.swift %t/Other.swift -I %S/Inputs/MemberImportVisibility -module-name Underlying -verify -swift-version 5 -enable-upcoming-feature MemberImportVisibility

// REQUIRES: swift_feature_MemberImportVisibility

//--- Other.swift
@_exported import Underlying

//--- Primary.swift

func test(_ s: UnderlyingStruct) {
  _ = s.a
}
