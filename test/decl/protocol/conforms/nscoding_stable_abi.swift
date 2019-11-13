// RUN: %empty-directory(%t)

// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %S/nscoding.swift -dump-ast -target %target-stable-abi-triple > %t/new.ast

// RUN: %FileCheck --check-prefix=NEGATIVE --check-prefix=NEGATIVE-NEW %S/nscoding.swift < %t/new.ast

// REQUIRES: objc_interop
// REQUIRES: swift_stable_abi
