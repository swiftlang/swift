// RUN: %target-swift-emit-silgen %s -verify

protocol P {
  var d: Double? { get }
}

// rdar://126583098 - Make sure this compiles.
func foo<each S: Sequence>(_ xss: (repeat (each S))) where repeat (each S).Element: P {
  var n: Double = 0
  for xs in repeat each xss {
    for x in xs {
      n += x.d ?? 0
    }
  }
}
