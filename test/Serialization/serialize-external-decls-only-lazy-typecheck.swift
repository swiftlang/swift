// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -enable-library-evolution -parse-as-library -package-name Package -typecheck -verify
// RUN: %target-swift-frontend -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -emit-module -emit-module-path %t/lazy_typecheck.swiftmodule -enable-library-evolution -parse-as-library -package-name Package -experimental-lazy-typecheck -experimental-skip-all-function-bodies -experimental-serialize-external-decls-only
// RUN: %target-swift-frontend -package-name Package -typecheck %s -I %t

import lazy_typecheck

struct ConformsToPublicProto: PublicProto {
  func req() -> Int { return 1 }
}

func testGlobalFunctions() {
  _ = publicFunc()
  _ = publicFuncWithDefaultArg()
  _ = packageFunc()
  constrainedGenericPublicFunction(ConformsToPublicProto())
  if #available(SwiftStdlib 5.1, *) {
    _ = publicFuncWithOpaqueReturnType()
    _ = publicAEICFuncWithOpaqueReturnType()
  }
}

func testPublicStruct(_ s: PublicStruct) {
  _ = s.publicMethod()
}
