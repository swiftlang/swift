@propertyWrapper
public struct Clamped<T: Comparable> {
    private var value: T
    let min: T
    let max: T
    
    public init(wrappedValue: T, min: T, max: T) {
        self.min = min
        self.max = max
        self.value = Swift.min(Swift.max(wrappedValue, min), max)
    }
    
    public var wrappedValue: T {
        get { value }
        set { value = Swift.min(Swift.max(newValue, min), max) }
    }
    
    public var projectedValue: Clamped<T> { self }
}
