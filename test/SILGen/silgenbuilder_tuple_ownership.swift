// RUN: %target-swift-emit-silgen %s
//
// Just make sure that we do not trigger the ownership verifier on this code. We
// were previously not emitting a destroy_value for (nil, error) since we were
// seeing the .none for [String: Any]? and propagating that values ownership
// rather than the error.

public enum Outcome<T> {
    case success(T)
    case error(T?, Error)
}

public protocol RequestContentRepresentable {
}

public class HttpClient {
    public func fetch(requestContent: RequestContentRepresentable, completionHandler: @escaping (Outcome<[String: Any]>) -> Void) throws {
      fatalError()
    }
}

public final class Future <ResultType> {
  @discardableResult
  public func finish(result: ResultType) -> Bool {
    fatalError()
  }
}

class Controller {
  internal func test() {
    let content2: RequestContentRepresentable? = nil
    let content = content2!
    let httpClient2: HttpClient? = nil
    let httpClient: HttpClient = httpClient2!
    
    // Create a Future to encapsulate the response handler.
    // This allows us to guarantee we only call it once.
    // We set the handler in the success block and we fail the future if we should no longer be allowed to call the completion
    let futureResponseHandler = Future<([String: Any]?, Error?)>()
  
    do {
      try httpClient.fetch(requestContent: content) { (outcome) in
      }
    } catch let error {
      // This is calling the future's success handler with a tuple.
      futureResponseHandler.finish(result: (nil, error))
    }
  }
}
