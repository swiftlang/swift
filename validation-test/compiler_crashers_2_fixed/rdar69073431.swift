// RUN: %target-swift-frontend -emit-ir %s
// REQUIRES: OS=macosx

import Combine

@available(macOS 10.15, *)
extension Publishers.Share {
    func foo<A: Publisher, B: Publisher>() where Upstream == Publishers.FlatMap<A, B>, B.Output == UInt8 {
        
    }
}
