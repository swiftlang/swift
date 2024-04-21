// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -verify -verify-additional-prefix swift-5-
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -verify -verify-additional-prefix default-to-internal-

//--- Lib.swift
public struct SomeType {}

//--- Client.swift
@_implementationOnly import Lib
// expected-swift-5-warning @-1 {{'@_implementationOnly' is deprecated, use 'internal import' and family instead}} {{1-21=internal}}
// expected-default-to-internal-warning @-2 {{'@_implementationOnly' is deprecated, use a bare import as 'InternalImportsByDefault' is enabled}} {{1-22=}}

internal func foo(_: SomeType) {}
