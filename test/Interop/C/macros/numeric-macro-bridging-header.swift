// RUN: %target-typecheck-verify-swift -enable-upcoming-feature MemberImportVisibility -import-objc-header %S/Inputs/numeric-macro-bridging-header.h %s
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature MemberImportVisibility -import-objc-header %S/Inputs/numeric-macro-bridging-header.h -cxx-interoperability-mode=default %s

// REQUIRES: swift_feature_MemberImportVisibility

let x = SOME_NUMERIC_CONSTANT
_ = x
