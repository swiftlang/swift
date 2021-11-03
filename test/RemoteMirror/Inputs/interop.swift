@_cdecl("test")
public func test() -> UInt {
  return unsafeBitCast(c, to: UInt.self)
}

enum E {
case one
case two
case three
}

enum F {
case one(AnyObject)
case two
case three
}

protocol Frobble {
  func frobble()
}

protocol Wibble {
  func wibble()
}

class FooErr: Error {}

class Foo: Frobble, Wibble {
  func frobble() {}
  func wibble() {}
}

// Class existential
enum G {
case one
case two(Foo)
case three
case four
}

// Error existential
enum H {
case one
case two
case three(Error)
case four
}

// Opaque existentials
enum I {
case one
case two
case three
case four(Frobble)
}

enum J {
case one(Frobble & Wibble)
case two
case three
case four
}

// Existential metatype
enum K {
case one
case two(Frobble.Type)
case three
case four
}

class C {
  let x = "123"
  let y = 456
  let e = E.two

  let f1 = F.one(Foo())
  let f2 = F.two
  let f3 = F.three

  let g1 = G.one
  let g2 = G.two(Foo())
  let g3 = G.three
  let g4 = G.four

  let h1 = H.one
  let h2 = H.two
  let h3 = H.three(FooErr())
  let h4 = H.four

  let i1 = I.one
  let i2 = I.two
  let i3 = I.three
  let i4 = I.four(Foo())

  let j1 = J.one(Foo())
  let j2 = J.two
  let j3 = J.three
  let j4 = J.four

  let k1 = K.one
  let k2 = K.two(Foo.self)
  let k3 = K.three
  let k4 = K.four
}

let c = C()
