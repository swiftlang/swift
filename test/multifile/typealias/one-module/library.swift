// RUN: true

public enum Result<T, U>
{
    case success(T)
    case failure(U)
}

public typealias GenericResult<T> = Result<T, Error>
