
public struct SimpleResilient {
    let x: Int
    let y: AnyObject

    public init(x: Int, y: AnyObject) {
        self.x = x
        self.y = y
    }
}

public struct GenericResilient<C, T> {
    let x: C
    let y: T

    public init(x: C, y: T) {
        self.x = x
        self.y = y
    }
}