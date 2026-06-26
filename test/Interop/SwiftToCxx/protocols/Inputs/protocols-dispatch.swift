// Swift protocol with methods for existential wrapper execution test.

public protocol Drawable {
    func draw() -> Int
}

public protocol Resizable {
    func resize(to factor: Int) -> Bool
}

public protocol Stylable: Drawable {
    func style() -> Bool
}

public struct Circle: Drawable, Resizable {
    var radius: Int
    public init(radius: Int) { self.radius = radius }
    public func draw() -> Int { return radius * radius }
    public func resize(to factor: Int) -> Bool { return factor > 0 && factor <= radius }
}

public struct StyledCircle: Stylable {
    var radius: Int
    public init(radius: Int) { self.radius = radius }
    public func draw() -> Int { return radius * radius * 3 }
    public func style() -> Bool { return radius > 5 }
}

// Thunks to create existentials from C++. The compiler will
// generate these automatically once Phase 1.5 (boxing constructors)
// is implemented. For now they are hand-written stubs.
@_cdecl("createCircleDrawable")
func createCircleDrawable(_ outPtr: UnsafeMutableRawPointer, _ radius: Int) {
    outPtr.assumingMemoryBound(to: (any Drawable).self)
        .initialize(to: Circle(radius: radius))
}

@_cdecl("createCircleResizable")
func createCircleResizable(_ outPtr: UnsafeMutableRawPointer, _ radius: Int) {
    outPtr.assumingMemoryBound(to: (any Resizable).self)
        .initialize(to: Circle(radius: radius))
}

@_cdecl("createStyledCircleStylable")
func createStyledCircleStylable(_ outPtr: UnsafeMutableRawPointer, _ radius: Int) {
    outPtr.assumingMemoryBound(to: (any Stylable).self)
        .initialize(to: StyledCircle(radius: radius))
}
