// RUN: %target-swift-frontend %s -emit-sil -Ounchecked -Xllvm -sil-disable-pass=cmo | %target-sil-opt -parse-serialized-sil

// Fails if the positions of the two Collection subscript requirements are
// reversed. rdar://problem/46650834
// UNSUPPORTED: swift_evolve

// FIXME(NCG): This produces `cannot suppress conformances here` errors due to
// all the new <τ_0_0 where τ_0_0 : ~Copyable> clauses
// XFAIL: !noncopyable_generics

var W = [UInt32](repeating: 0, count: 16)
