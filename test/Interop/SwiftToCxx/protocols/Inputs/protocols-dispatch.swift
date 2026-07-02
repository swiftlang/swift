public protocol Drawable {
    func draw() -> Int
}

public protocol Resizable {
    func resize(to factor: Int) -> Bool
}

public protocol Stylable: Drawable {
    func style() -> Bool
}

public protocol Container<Element> {
    associatedtype Element
    func count() -> Int
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

public struct IntArray: Container {
    public typealias Element = Int
    var n: Int
    public init(count: Int) { self.n = count }
    public func count() -> Int { return n }
}

public func drawTwice(_ d: any Drawable) -> Int {
    return d.draw() + d.draw()
}

public func bestDrawable(_ a: any Drawable, _ b: any Drawable) -> any Drawable {
    return a.draw() >= b.draw() ? a : b
}
