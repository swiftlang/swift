enum E<Value> {
    case s(Value)
    case n
}

public struct S<Value> {
    var e: E<Value>
    var b: Bool = false

    public init() {
        self.e = .n
    }
}


