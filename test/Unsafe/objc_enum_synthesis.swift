// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -Wwarning StrictMemorySafety
// RUN: %target-typecheck-verify-swift -strict-memory-safety
// RUN: %target-typecheck-verify-swift -D FILE_DEFAULT
// RUN: %target-typecheck-verify-swift -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros
// RUN: %target-typecheck-verify-swift -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -Wwarning StrictMemorySafety
// RUN: %target-typecheck-verify-swift -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -strict-memory-safety
// RUN: %target-typecheck-verify-swift -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -D FILE_DEFAULT

// REQUIRES: objc_interop
// REQUIRES: swift_feature_DeriveConformancesViaMacros

// The synthesized 'rawValue' bit-casts 'self', and acknowledges that with 'unsafe'.

#if FILE_DEFAULT
default @diagnose(StrictMemorySafety, as: error)
#endif

@objc enum ObjCEnum: Int {
  case a, b
}

func use(_ value: ObjCEnum) -> Int {
  value.rawValue
}
