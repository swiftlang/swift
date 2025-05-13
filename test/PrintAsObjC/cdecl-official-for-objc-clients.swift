/// Similar test to cdecl-official but gated to objc-interop compatibility

// RUN: %empty-directory(%t)
// RUN: split-file %S/cdecl-official.swift %t --leading-lines

/// Generate cdecl.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %t/Lib.swift -emit-module -verify -o %t -emit-module-doc \
// RUN:   -emit-clang-header-path %t/cdecl.h \
// RUN:   -enable-experimental-feature CDecl

/// Check cdecl.h directly
// RUN: %check-in-clang %t/cdecl.h
// RUN: %check-in-clang-cxx %t/cdecl.h

/// Build an Objective-C client against cdecl.h
// RUN: %clang -c %t/Client.c -fmodules -I %t \
// RUN:   -F %S/../Inputs/clang-importer-sdk-path/frameworks \
// RUN:   -I %clang-include-dir -Werror \
// RUN:   -isysroot %S/../Inputs/clang-importer-sdk

// REQUIRES: swift_feature_CDecl
// REQUIRES: objc_interop
