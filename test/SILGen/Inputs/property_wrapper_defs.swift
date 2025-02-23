@propertyWrapper
public struct MyPublished<Value> {
	private var stored: Value

	public var wrappedValue: Value {
		get { stored }
		set { stored = newValue }
	}		

	public init(wrappedValue initialValue: Value) {
		stored = initialValue
	}

	public var projectedValue: Self {
		mutating get { self }
		set { self = newValue }
	}
}

public class UsesMyPublished {
  @MyPublished public var foo: Int = 17
}
