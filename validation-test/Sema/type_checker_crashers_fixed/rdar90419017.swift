// RUN: %target-typecheck-verify-swift

public enum APIError {
case unknown
case message
}

struct NewItemResponse {}

protocol Publisher {
  associatedtype Output // expected-note {{protocol requires nested type 'Output'}}
}

extension Publisher {
  func sink(receiveCompletion: @escaping ((Int) -> Void), receiveValue: @escaping ((Self.Output) -> Void)) { fatalError() }
  // expected-note@-1 {{'sink(receiveCompletion:receiveValue:)' declared here}}
}

func fetchFile<T>(name: String) -> MyPublisher<T, APIError> {
  fatalError()
}

struct ReplaceError<Upstream> : Publisher { 
  // expected-error@-1 {{type 'ReplaceError<Upstream>' does not conform to protocol 'Publisher'}}
  // expected-note@-2 {{add stubs for conformance}}
  typealias Output = Upstream.Output // expected-error {{'Output' is not a member type of type 'Upstream'}}
  typealias Failure = Never
}

struct MyPublisher<Output, Failure> : Publisher {
  init<P>(_ publisher: P) { fatalError() }

  func replaceError(with output: Self.Output) -> ReplaceError<Self> { fatalError() }
}

public class Items {
  func test() {
    _ = fetchFile(name: "abc")
          .replaceError(with: NewItemResponse())
          .sink { [weak self] items in } // expected-error {{missing argument for parameter 'receiveValue' in call}}
  }
}
