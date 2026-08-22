// RUN: not %target-swift-frontend -typecheck -D PACK %s 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=PACK
// RUN: not %target-swift-frontend -typecheck -D INFERENCE %s 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=INFERENCE

protocol P {}
protocol Q {}

#if PACK
func expand<each Element>(_ elements: repeat (each Element).Type) {
  var index = 0
  func unpack<T>(_: T.Type) where T.Type: P {
    defer { index += 1 }
    _ = index
  }

  repeat unpack((each Element).self)
}
#endif

#if INFERENCE
func make<T>() -> T where T.Type: P {
  fatalError()
}

_ = make() as? any Q
#endif

// PACK: error: type '(each Element).Type' cannot conform to 'P'
// PACK: note: required by local function 'unpack'
// INFERENCE: error: generic parameter 'T' could not be inferred
// INFERENCE: note: in call to function 'make()'
