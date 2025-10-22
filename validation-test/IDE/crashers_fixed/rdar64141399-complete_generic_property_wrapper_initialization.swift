// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE_GENERIC -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE_IN_GENERIC_CONTEXT -source-filename=%s

struct Foo {
  static let bar: Foo
}

@propertyWrapper public struct GenericWrapper<Value> {
  public var wrappedValue: Value
  public var projectedValue: Int

  public init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
    self.projectedValue = 1
  }
}

public struct GenericContext<T> {
  @propertyWrapper public struct GenericWrapper<Value> {
    public var wrappedValue: Value
    public var projectedValue: Int

    public init(wrappedValue: Value) {
      self.wrappedValue = wrappedValue
      self.projectedValue = 1
    }
  }
}

public struct MyStruct {
  @GenericWrapper var someProperty = #^COMPLETE_GENERIC^#
}
public struct MyStruct2 {
  @GenericContext<Foo>.GenericWrapper var someProperty2 = #^COMPLETE_IN_GENERIC_CONTEXT^#
}
