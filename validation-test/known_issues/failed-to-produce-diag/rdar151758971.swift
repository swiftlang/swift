// rdar://151758971
// RUN: %target-typecheck-verify-swift

protocol Publisher<Output, Failure> {
  associatedtype Output
  associatedtype Failure: Error
}

protocol View {
  associatedtype Body: View
  var body: Body { get }
}

struct Text: View {
  init(_: String) {}
  var body: Never { fatalError() }
}

extension Never: View {
  var body: Never { fatalError() }
}

extension View {
  func onReceive<P: Publisher>(
    _ publisher: P, perform action: @escaping (P.Output) -> Void
  ) -> some View where P.Failure == Never { self }
}

struct BusyRebuildView: View {
  var body: some View {
    Text("")
    .onReceive(accountDonePublisher) { aValue in
    // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
    // expected-error@-2 {{cannot find 'accountDonePublisher' in scope}}
      let aString = aValue.name
      print(aString)
    }
  }
}
