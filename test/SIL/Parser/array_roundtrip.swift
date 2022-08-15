// RUN: %target-swift-frontend %s -emit-sil -Ounchecked -Xllvm -sil-disable-pass=cmo | %target-sil-opt -parse-serialized-sil

// Fails if the positions of the two Collection subscript requirements are
// reversed. rdar://problem/46650834
// UNSUPPORTED: swift_evolve

var W = [UInt32](repeating: 0, count: 16)
