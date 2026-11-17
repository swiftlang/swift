// rdar://117877417 (https://github.com/swiftlang/swift/issues/61039)
// RUN: %target-typecheck-verify-swift

func open(_ path: UnsafePointer<CChar>, _ oflag: Int32) -> Int32 { 0 }
func open(_ path: UnsafePointer<CChar>, _ oflag: Int32, _ mode: UInt16) -> Int32 { 0 }

struct Angle {
  static func degrees(_ degrees: Double) -> Angle { Angle() }
}

struct Image {
  func rotationEffect(_ angle: Angle) -> Image { self }
}

func test(image: Image) {
  _ = image.rotationEffect(.degrees(open ? 45 : 0)) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
