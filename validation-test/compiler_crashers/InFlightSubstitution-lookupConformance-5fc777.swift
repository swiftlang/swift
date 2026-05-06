// {"kind":"typecheck","original":"461251a7","signature":"swift::InFlightSubstitution::lookupConformance(swift::Type, swift::ProtocolDecl*, unsigned int)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
protocol c {
  associatedtype d: e where d.f == Self, d.g == b
  protocol e {
    associatedtype f: a
    associatedtype g
  }
  struct h: c {
    typealias d = i<h>
    struct i<j: c>: e {
      typealias f = j
      typealias g = j.b
    }
  }
}
