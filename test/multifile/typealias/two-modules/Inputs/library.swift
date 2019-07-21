public enum Result<T, U>
{
    case success(T)
    case failure(U)
}

public typealias GenericResult<T> = Result<T, Error>

public protocol Rdar46103190 {}
extension Rdar46103190 {
  public typealias Alias = String
  public typealias Map<K: Hashable> = [K: Self]
}
