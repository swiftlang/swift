// RUN: %target-swift-frontend %s -emit-sil -Ounchecked | %target-sil-opt -assume-parsing-unqualified-ownership-sil

// Fails if the positions of the two Collection subscript requirements are
// reversed. rdar://problem/46650834
// XFAIL: swift_evolve

var W = [UInt32](repeating: 0, count: 16)
