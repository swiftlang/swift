@available(DynamicDomain)
public struct X {
    public init() { }
}

public struct Z {
    public init() {
        if #available(DynamicDomain) {
            print("#available")
            print(X())
        } else {
            print("#unavailable")
        }
    }
}
