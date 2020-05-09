public typealias Alias<T> = (T) -> ()

public class Super {
  public func foo(_ f: @escaping Alias<Bool>) {}
}
