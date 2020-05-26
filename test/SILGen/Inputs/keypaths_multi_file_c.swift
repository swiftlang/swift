public protocol PP {}
public protocol QQ {}

public class AA : PP {}
public class BB : AA {}

public struct DD<T: PP> {}

public struct CC<T: PP> {
  public var x: DD<T> { fatalError("death") }
}
