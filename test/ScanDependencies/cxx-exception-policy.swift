// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -scan-dependencies %t/client.swift -module-name Client -I %t -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging -module-cache-path %t/cache -o %t/strict.json
// RUN: %FileCheck %s --check-prefix=STRICT --implicit-check-not='"swift": "CxxStdlib"' --implicit-check-not='"linkName": "swiftCxxStdlib"' < %t/strict.json
// RUN: %target-swift-frontend -typecheck %t/client.swift -I %t -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging -module-cache-path %t/cache
// RUN: %target-swift-frontend -emit-ir %t/client.swift -I %t -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging -module-cache-path %t/cache -o %t/client.ll
// RUN: %FileCheck %s --check-prefix=AUTOLINK --implicit-check-not=swiftCxxStdlib < %t/client.ll
// RUN: %target-swift-frontend -emit-module %t/overlay.swift -module-name CxxStdlib -o %t/ExplicitOverlay.swiftmodule
// RUN: not %target-swift-frontend -typecheck %t/client.swift -I %t -swift-module-file=CxxStdlib=%t/ExplicitOverlay.swiftmodule -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging 2>&1 | %FileCheck %s --check-prefix=OVERLAY

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

// Strict mode keeps the source spelling for the raw Clang standard library.
// Its Swift overlay contains helpers compiled for annotated exception policy.
// STRICT: "mainModuleName": "Client"
// STRICT-DAG: "clang": "std_ExceptionPolicyTest"
// STRICT-DAG: "clang": "std"
// AUTOLINK: swiftCxx
// OVERLAY: the CxxStdlib Swift overlay is unavailable in strict C++ exception mode

//--- module.modulemap
module std_ExceptionPolicyTest [system] {
  header "library.h"
  requires cplusplus
  export *
}
module HeaderLibrary {
  header "wrapper.h"
  requires cplusplus
  export *
}

//--- library.h
namespace std {
inline int exceptionPolicyValue() noexcept { return 42; }
}

//--- wrapper.h
#include "library.h"

//--- client.swift
import HeaderLibrary
import CxxStdlib
let value = std.exceptionPolicyValue()
#if !canImport(CxxStdlib)
#error("strict mode must allow the raw Clang standard library")
#endif

//--- overlay.swift
public func annotatedOverlayFunction() {}
