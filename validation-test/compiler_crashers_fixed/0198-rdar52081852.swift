// RUN: not %target-swift-frontend -typecheck %s
@propertyWrapper
public struct Wrapper<T> {
    public var wrappedV: T // part-way through typing wrappedValue
    
    public init(initialValue: T) {
        self.value = initialValue
    }
    
    public init(body: () -> T) {
        self.value = body()
    }
}

var globalInt: Int { return 17 }

public struct HasWrappers {
    @Wrapper
    public var x: Int = globalInt
    
    @Wrapper(body: { globalInt })
    public var y: Int
    
    @Wrapper(body: {
        struct Inner {
            @Wrapper
            var x: Int = globalInt
        }
        return Inner().x + globalInt
    })
    public var z: Int
    
    func backingUse() {
        _ = $y.value + $z.value + x + $x.value
    }
}

func useMemberwiseInits(i: Int) {
    _ = HasWrappers(x: i)
    _ = HasWrappers(y: Wrapper(initialValue: i))
}
