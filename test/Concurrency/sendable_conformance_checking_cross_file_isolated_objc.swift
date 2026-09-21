// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t -enable-objc-interop -parse-as-library -swift-version 5
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t -enable-objc-interop -parse-as-library -swift-version 5 -strict-concurrency=complete
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t -enable-objc-interop -parse-as-library -swift-version 6

// REQUIRES: concurrency
// REQUIRES: objc_interop

//--- module.modulemap
module IsolatedObjC {
  header "IsolatedObjC.h"
}

//--- IsolatedObjC.h
__attribute__((objc_root_class))
__attribute__((__swift_attr__("@MainActor")))
@interface IsolatedObjCClass
@end

__attribute__((objc_root_class))
__attribute__((__swift_attr__("@MainActor")))
@interface IsolatedObjCBase
@end

@interface InheritsObjCIsolation : IsolatedObjCBase
@end

__attribute__((objc_root_class))
__attribute__((__swift_attr__("@MainActor")))
@interface RefinedObjCClass
@end

//--- main.swift
import IsolatedObjC

protocol RefinesSendable: Sendable {}

extension IsolatedObjCClass: Sendable {}
// expected-warning@-1:1 {{extension declares a conformance of imported type 'IsolatedObjCClass' to imported protocol 'Sendable'; this will not behave correctly if the owners of 'IsolatedObjC' introduce this conformance in the future}}
// expected-note@-2 {{add '@retroactive' to silence this warning}}
// expected-warning@-3:1 {{conformance to 'Sendable' must occur in the same source file as class 'IsolatedObjCClass'; use '@unchecked Sendable' for retroactive conformance; this will be an error in a future Swift language mode}}

extension InheritsObjCIsolation: Sendable {}
// expected-warning@-1:1 {{extension declares a conformance of imported type 'InheritsObjCIsolation' to imported protocol 'Sendable'; this will not behave correctly if the owners of 'IsolatedObjC' introduce this conformance in the future}}
// expected-note@-2 {{add '@retroactive' to silence this warning}}
// expected-warning@-3:1 {{conformance to 'Sendable' must occur in the same source file as class 'InheritsObjCIsolation'; use '@unchecked Sendable' for retroactive conformance; this will be an error in a future Swift language mode}}

// Clang imported + implied is totally ignored, not sure this is correct but its the existing behavior.
extension RefinedObjCClass: RefinesSendable {}

func requiresSendable<T: Sendable>(_: T.Type) {}
func testStillSendable() {
  requiresSendable(IsolatedObjCClass.self)
  requiresSendable(InheritsObjCIsolation.self)
  requiresSendable(RefinedObjCClass.self)
}
