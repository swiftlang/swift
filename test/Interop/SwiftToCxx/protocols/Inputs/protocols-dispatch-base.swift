public protocol Drawable {
    func draw() -> Int
}

public protocol Resizable {
    func resize(to factor: Int) -> Bool
}

public protocol Stylable: Drawable {
    func style() -> Bool
}

public protocol Renderable: AnyObject {
    func render() -> Int
}

public protocol MultiReq {
    func first() -> Int
    func second() -> Int
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

public class Canvas: Renderable {
    var size: Int
    public init(size: Int) { self.size = size }
    public func render() -> Int { return size * 2 }
}

public struct Pair: MultiReq {
    var a: Int
    var b: Int
    public init(a: Int, b: Int) { self.a = a; self.b = b }
    public func first() -> Int { return a }
    public func second() -> Int { return b }
}

public struct LargeDrawable: Drawable {
    var a: Int
    var b: Int
    var c: Int
    var d: Int
    public init(a: Int, b: Int, c: Int, d: Int) {
        self.a = a; self.b = b; self.c = c; self.d = d
    }
    public func draw() -> Int { return a + b + c + d }
}
