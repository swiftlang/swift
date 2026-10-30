// RUN: %target-swift-frontend %s -emit-sil -verify -enable-experimental-feature Lifetimes -disable-availability-checking

// REQUIRES: swift_feature_Lifetimes

// rdar://178954550 - The constraint solver must carry the contextual ownership
// specifier onto anonymous / unannotated closure parameters, so an inferred
// `consuming` parameter is mutable in the closure body just like one that was
// spelled out explicitly.

struct Mut {
  var x: Int = 0
  var y: Int = 0
}

func takeConsuming(_ body: (consuming Mut) -> Void) {}
func takeBorrowing(_ body: (borrowing Mut) -> Void) {}
func takeInout(_ body: (inout Mut) -> Void) {}


takeConsuming { $0.x += 5 }

takeConsuming { mut in mut.x += 8 }


takeBorrowing { $0.x += 1 }
// expected-error@-1 {{left side of mutating operator isn't mutable: '$0' is immutable}}
takeBorrowing { b in b.x += 1 }
// expected-error@-1 {{left side of mutating operator isn't mutable: 'b' is a 'let' constant}}


takeInout { $0.x += 1 }
takeInout { m in m.x += 1 }

// MutableSpan taken as consuming should be mutable.
struct Holder: ~Escapable, ~Copyable {
  var mutableSpan: MutableSpan<Int>
  @_lifetime(copy span)
  init(_ span: consuming MutableSpan<Int>) { self.mutableSpan = span }
}

func takeConsumingHolder(_ body: (consuming Holder) -> Void) {}

takeConsumingHolder { $0.mutableSpan[0] = 5 }
takeConsumingHolder { h in h.mutableSpan[0] = 5 }
