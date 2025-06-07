// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -verify
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -verify

// REQUIRES: swift_feature_InternalImportsByDefault

//--- Lib.swift
public struct SomeType {}

//--- Client.swift
@_implementationOnly import Lib
// expected-warning @-1 {{'@_implementationOnly' is deprecated, use 'internal import' instead}} {{1-21=internal}}

internal func foo(_: SomeType) {}
