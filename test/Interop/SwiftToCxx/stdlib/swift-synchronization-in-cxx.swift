// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -module-name UseSynchronization -enable-experimental-cxx-interop -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/UseSynchronization.h
// RUN: %FileCheck %s < %t/UseSynchronization.h

// REQUIRES: synchronization

import Synchronization

public class SomeClass { }

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, visionOS 2, *)
public class Thing {
    public static let rc = Mutex(SomeClass())
}

// CHECK: class SWIFT_AVAILABILITY(visionos,introduced=2) SWIFT_AVAILABILITY(watchos,introduced=11) SWIFT_AVAILABILITY(tvos,introduced=18) SWIFT_AVAILABILITY(ios,introduced=18) SWIFT_AVAILABILITY(macos,introduced=15) SWIFT_SYMBOL("s:18UseSynchronization5ThingC") Thing;
