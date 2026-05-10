// RUN: %target-typecheck-verify-swift

protocol ResultType {
  associatedtype E
  associatedtype F

  func failure() -> F?
}

extension Result: ResultType {
  typealias E = Success
  typealias F = Failure

  func failure() -> F? { fatalError() }
}

protocol ObservableType {
    associatedtype Element
}

class Observable<Element> : ObservableType {
  func flatMap<Source: ObservableType>(_ selector: @escaping (Element) throws -> Source) -> Observable<Source.Element> {
    fatalError()
  }

  static func just(_ element: Element) -> Observable<Element> {
    fatalError()
  }
}

extension Observable where Element: ResultType {
  func mapGenericError() -> Observable<Result<Element.E, Error>> {
    flatMap { result -> Observable<Result<Element.E, Error>> in
      if let error = result.failure() as? Error {
        return .just(Result.failure(error)) // Ok
      }
      fatalError()
    }
  }
}
