// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/library.swift -module-name ThrowingLibrary -emit-module-path %t/Modules/ThrowingLibrary.swiftmodule -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -typecheck -verify %t/disabled.swift -I %t/Modules -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck -verify %t/disabled.swift -I %t/Modules -cxx-interoperability-mode=default -experimental-allow-module-with-compiler-errors
// RUN: %target-swift-frontend -typecheck %t/enabled.swift -I %t/Modules -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -emit-module %t/library.swift -module-name ThrowingLibrary -emit-module-path %t/Resilient/ThrowingLibrary.swiftmodule -enable-library-evolution -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -typecheck -verify %t/disabled.swift -I %t/Resilient -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -emit-module %t/library.swift -module-name ThrowingLibrary -emit-module-path %t/NoCxxRequirement/ThrowingLibrary.swiftmodule -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -disable-cxx-interop-requirement-at-import
// RUN: %target-swift-frontend -typecheck -verify %t/disabled.swift -I %t/NoCxxRequirement -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck -verify %t/disabled.swift -I %t/NoCxxRequirement -cxx-interoperability-mode=default -experimental-allow-module-with-compiler-errors
// RUN: %target-swift-frontend -emit-module %t/plain.swift -module-name PlainLibrary -emit-module-path %t/Plain/PlainLibrary.swiftmodule -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck %t/plain-client.swift -I %t/Plain -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck %t/plain-client.swift -I %t/Plain -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -emit-module %t/plain.swift -module-name PlainLibrary -emit-module-path %t/NoCxx/PlainLibrary.swiftmodule -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -typecheck %t/plain-client.swift -I %t/NoCxx

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

// The feature-disabled client deliberately cannot find the C++ dependency.
// Diagnose the required feature before loading dependencies or declarations.
// Ordinary C++ modules remain importable with the feature enabled or disabled.

//--- Inputs/module.modulemap
module ThrowingFunctions {
  header "functions.h"
  requires cplusplus
}

//--- Inputs/functions.h
inline int checkedValue() __attribute__((swift_attr("import_throws"))) {
  return 42;
}

//--- library.swift
import ThrowingFunctions

public func value() throws -> CInt {
  try checkedValue()
}

//--- disabled.swift
import ThrowingLibrary // expected-error {{module 'ThrowingLibrary' requires '-enable-experimental-feature CxxExceptionBridging'}}

//--- enabled.swift
import ThrowingLibrary

func useValue() throws -> CInt {
  try value()
}

//--- plain.swift
public func answer() -> Int { 42 }

//--- plain-client.swift
import PlainLibrary
let result = answer()
