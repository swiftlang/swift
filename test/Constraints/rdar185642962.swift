// RUN: %target-swift-emit-silgen -verify %s

// We constructed an Array with a non-escaping function type as its
// element type, and this made it all the way to SILGen. The code
// is valid, but we should not infer non-escaping for that binding.
// Make sure it survives to SILGen with no warnings or errors.

let x = [{ "" }]

func f<C>(_: () -> C, _: (C.Element) -> Void) where C: Collection {}

func g() {
  f(
    { x },
    { (f: () -> String) in }
  )
}
