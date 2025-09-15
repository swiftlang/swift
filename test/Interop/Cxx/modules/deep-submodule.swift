// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default
// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -enable-upcoming-feature MemberImportVisibility
// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -D IMPORT_TOP_LEVEL
// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -D IMPORT_ANOTHER_SUBMODULE

// REQUIRES: swift_feature_MemberImportVisibility

#if IMPORT_TOP_LEVEL
import TopLevelModule
#elseif IMPORT_ANOTHER_SUBMODULE
import TopLevelModule.SubModule.AnotherDeepSubModule
#else
import TopLevelModule.SubModule.DeepSubModule
#endif

let _: NS.MyStructInDeepSubModule! = nil

extension NS.MyStructInDeepSubModule {
  public static func takesInstance(_ i: NS.MyStructInDeepSubModule) {}
}
