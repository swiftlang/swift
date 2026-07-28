// FIXME: crashes under opaque values
// RUN: not --crash %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -disable-availability-checking %s

// RUN: %target-swift-frontend -emit-silgen %s -disable-availability-checking

protocol P {
  associatedtype A: P
}

struct S: P {
  typealias A = S
}

func f<each T: P>(_: repeat each T) -> (repeat (each T).A.A.A.A) {}

f(S(), S(), S())
