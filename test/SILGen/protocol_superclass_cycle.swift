// RUN: %target-swift-frontend -emit-silgen %s

// When a protocol has a superclass requirement on 'Self' and the class
// itself conforms concretely, the forwarding substitution map will have
// a concrete conformance in it. Make sure this is handled correctly
// without tripping the SIL verifier.

class C: P {}

protocol P: C {}

func f<T : P>(_ t: T) {
  // The weak reference causes us to form a SILBoxType, which is where
  // the problematic conformance popped up.
  _ = { [weak t] in _ = t }
}
