import RoundTrip

protocol First {
  associatedtype Assoc : First

  // Just to confuse things -- a method with the same name as an
  // associated type
  func Assoc(_: Int) -> Int
}

protocol Second {
  associatedtype Assoc : Second
}

struct OuterFirst<A : First, B : First> {
  struct Inner<C : First, D : First> {
    func method(a: A, b: B, c: C, d: D) {
      do {
        let _: (A, A.Assoc, A.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let _: (B, B.Assoc, B.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let _: (C, C.Assoc, C.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let _: (D, D.Assoc, D.Assoc.Assoc) -> () = { _, _, _ in }
      }
    }
  }
}

struct OuterBoth<A : First & Second, B : First & Second> {
  struct Inner<C : First & Second, D : First & Second> {
    func method(a: A, b: B, c: C, d: D) {
      do {
        let _: (A, A.Assoc, A.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let _: (B, B.Assoc, B.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let _: (C, C.Assoc, C.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let _: (D, D.Assoc, D.Assoc.Assoc) -> () = { _, _, _ in }
      }
    }
  }
}

struct F1: First {
  typealias Assoc = F2

  func Assoc(_ x: Int) -> Int { return x + 1 }
}

struct F2: First {
  typealias Assoc = F1

  func Assoc(_ x: Int) -> Int { return x * 2 }
}

struct FS1: First, Second {
  typealias Assoc = FS2

  func Assoc(_ x: Int) -> Int { return x - 1 }
}

struct FS2: First, Second {
  typealias Assoc = FS1

  func Assoc(_ x: Int) -> Int { return x / 2 }
}

public func test() {
  roundTripType(OuterFirst<F1,F2>.self)
  roundTripType(OuterBoth<FS1,FS2>.self)
  roundTripType(OuterFirst<F1,F2>.Inner<F2,F1>.self)
  roundTripType(OuterBoth<FS1,FS2>.Inner<FS2,FS1>.self)
  roundTripType(type(of:OuterFirst<F1,F2>.Inner<F2,F1>.method))
  roundTripType(type(of:OuterBoth<FS1,FS2>.Inner<FS2,FS1>.method))
}
