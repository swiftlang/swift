// swift/EmbeddedPlatform.h lives in the Clang resource directory,
// so neither clang nor swiftc needs -I to find it.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %clang -fsyntax-only -x c %t/header.h
// RUN: %target-swift-frontend -typecheck %t/test.swift \
// RUN:   -enable-experimental-feature Embedded -plugin-path %swift-plugin-dir \
// RUN:   -import-bridging-header %t/header.h

// REQUIRES: swift_feature_Embedded

//--- header.h
#include <swift/EmbeddedPlatform.h>

//--- test.swift
let _: CInt = EMBEDDED_SWIFT_PLATFORM_VERSION_MAJOR
