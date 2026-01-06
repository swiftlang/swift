// RUN: %target-swift-frontend -emit-sil -verify %s

func foo<T>(a: Bool, t: T) {
  let x: T
  defer { print(x) }

  x = t
  return
}

func bar<T>(a: Bool, t: T) {
  let x: T // expected-note {{defined here}}
  defer { print(x) } //expected-error{{constant 'x' used before being initialized}}

  if a {
    x = t
    return
  }
}

func bas<T>(a: Bool, t: T) {
  let x: T 
  defer { print(x) }

  if a {
    x = t
    return
  }

  x = t
}

func quz<T>(a: Bool, t: T) {
  let closure: (inout T) -> Void = { _ in }
  var x: T // expected-note {{defined here}}
  defer { closure(&x) } // expected-error{{variable 'x' passed by reference before being initialized}}
  if a {
    x = t
    return
  }
}
