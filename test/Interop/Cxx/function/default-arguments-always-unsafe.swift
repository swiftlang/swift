// RUN: %target-swift-emit-silgen %s -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportUnsafeCxxMethodsAsAlwaysUnsafe | %FileCheck %s

// REQUIRES: swift_feature_ImportUnsafeCxxMethodsAsAlwaysUnsafe

// A C++ default argument is lowered to a generator function whose name is
// derived from the Clang declaration, so importing the same method twice -- once
// under its original name and once as the '__<name>Unsafe' migration stub --
// has to reuse one generator. Synthesizing a second one fails this RUN line with
// "error: multiple definitions of symbol '$sSC...__defaultArg_0_...'".

import DefaultArguments

public func viaOriginalName(
  _ h: inout SelfContainedHasMethodWithDefaultArg
) -> UnsafeMutablePointer<CInt>? {
  return unsafe h.withDefault()
}

public func viaStub(
  _ h: inout SelfContainedHasMethodWithDefaultArg
) -> UnsafeMutablePointer<CInt>? {
  return unsafe h.__withDefaultUnsafe()
}

// CHECK-DAG: sil {{.*}}15viaOriginalName
// CHECK-DAG: sil {{.*}}7viaStub
// CHECK-DAG: sil {{.*}}[export_implementation] {{.*}}__defaultArg_0_
