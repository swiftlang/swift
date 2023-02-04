@runtimeMetadata
public struct Ignore {
  public init<T>(attachedTo: T,
                 fileID: String = #fileID,
                 line: Int = #line,
                 column: Int = #column) {}
}

@Ignore
public protocol Ignorable {}

public class Base : Ignorable {
  public init() {}
}

public class Child : Base {
}

@available(*, unavailable)
@Ignore
extension Child {}
