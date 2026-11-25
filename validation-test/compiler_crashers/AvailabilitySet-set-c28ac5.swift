// {"kind":"emit-sil","signature":"(anonymous namespace)::AvailabilitySet::set(unsigned int, (anonymous namespace)::DIKind)","signatureAssert":"Assertion failed: (Idx < size() && \"Out-of-bounds Bit access.\"), function operator[]","signatureNext":"DefiniteInitialization::run"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
struct a {
  var b: ()
  var c: (a, ()) {
    init {
    }
    get {
    }
  }
  init(d: a) {
    c = (d, ())
  }
}
