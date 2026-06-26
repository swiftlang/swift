// Swift protocol + conforming types for existential wrapper POC.
//
// The @_cdecl functions below are hand-written stubs that stand in
// for the extern "C" thunks the compiler (PrintAsClang) will
// generate automatically. They will not appear in the real
// implementation -- they exist only to validate that the
// SwiftExistentialType base class correctly manages the existential
// container lifecycle (copy, assign, destroy via VWT).

public protocol Drawable {
    func draw() -> Int
}

public struct Circle: Drawable {
    var radius: Int
    public init(radius: Int) { self.radius = radius }
    public func draw() -> Int { return radius * radius }
}

public struct Square: Drawable {
    var side: Int
    public init(side: Int) { self.side = side }
    public func draw() -> Int { return side * side * 4 }
}

// Stub: fill an existential container with a Circle value.
@_cdecl("createCircleDrawable")
func createCircleDrawable(_ outPtr: UnsafeMutableRawPointer, _ radius: Int) {
    let ptr = outPtr.assumingMemoryBound(to: (any Drawable).self)
    ptr.initialize(to: Circle(radius: radius))
}

@_cdecl("createSquareDrawable")
func createSquareDrawable(_ outPtr: UnsafeMutableRawPointer, _ side: Int) {
    let ptr = outPtr.assumingMemoryBound(to: (any Drawable).self)
    ptr.initialize(to: Square(side: side))
}

// Stub: call draw() on the existential.
@_cdecl("drawableCallDraw")
func drawableCallDraw(_ existentialPtr: UnsafeRawPointer) -> Int {
    return existentialPtr.assumingMemoryBound(to: (any Drawable).self).pointee.draw()
}

// Verify existential layout matches what C++ expects.
@_cdecl("getDrawableExistentialSize")
func getDrawableExistentialSize() -> Int {
    return MemoryLayout<any Drawable>.size
}

@_cdecl("getDrawableExistentialAlignment")
func getDrawableExistentialAlignment() -> Int {
    return MemoryLayout<any Drawable>.alignment
}
