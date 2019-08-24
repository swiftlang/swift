// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules/ -enable-objc-interop -import-objc-header %S/Inputs/submodules-bridging-header.h %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-pch -o %t/submodules-bridging-header.pch %S/Inputs/submodules-bridging-header.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules/ -enable-objc-interop -import-objc-header %t/submodules-bridging-header.pch %s

// From ctypes.bits submodule
public var x : DWORD = MY_INT
public var y : CInt = x
public var z : DWORD = "" // expected-error {{cannot convert value}}

