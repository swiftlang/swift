@propertyWrapper
public struct Doubled {
    private var value: Int
    public var wrappedValue: Int {
        get { value }
        set { value = newValue * 2 }
    }
    public init(wrappedValue: Int) {
        value = wrappedValue * 2
    }
}
