public struct Y {
    init() {
        if #available(DynamicDomain) {
            print("#available")
            print(X())
            print(Z())
        } else {
            print("#unavailable")
            print(Z())
        }
    }
}
