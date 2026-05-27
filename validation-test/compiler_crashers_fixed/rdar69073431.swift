// RUN: %target-swift-frontend -emit-ir %s -debug-generic-signatures 2>&1 | %FileCheck %s
// REQUIRES: OS=macosx

import Combine

@available(macOS 10.15, *)
extension Publishers.Share {
    // CHECK-LABEL: .foo()@
    // CHECK-NEXT: Generic signature: <Upstream, A, B where Upstream == Publishers.FlatMap<A, B>, A : Publisher, B : Publisher, A.[Publisher]Failure == B.[Publisher]Failure, B.[Publisher]Output == UInt8>
    func foo<A: Publisher, B: Publisher>() where Upstream == Publishers.FlatMap<A, B>, A : Publisher, B : Publisher, B.Output == UInt8 {
        
    }
}
