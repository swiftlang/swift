// RUN: %target-swift-frontend -emit-ir -g %s

protocol P {
  associatedtype Failure: Error
  func f() -> G<Failure>
}

struct G<T> {}

struct MyError: Error {}

struct Opaque1: P {
  func f() -> G<some Error> {
    return G<Never>()
  }
}

struct Opaque2: P {
  func f() -> G<some Error> {
    return G<MyError>()
  }
}

extension P where Failure == Never {
  func f1() throws(Failure) {}
}

extension P where Failure == Opaque1.Failure {
  func f2() throws(Failure) {}
}

extension P where Failure == Opaque2.Failure {
  func f3() throws(Failure) {}
}

func g<T: P>(_ t: T) throws where T.Failure == Never {
  try t.f1()
}

func g<T: P>(_ t: T) throws where T.Failure == Opaque1.Failure {
  // FIXME: We still crash while generating the call to f2().
  // try t.f2()
}

func g3<T: P>(_ t: T) throws where T.Failure == Opaque2.Failure {
  // FIXME: We still crash while generating the call to f3().
  // try t.f3()
}