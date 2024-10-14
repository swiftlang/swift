import _Concurrency

@available(SwiftStdlib 9999, *)
extension Observable where Self: AnyObject {
   /// An asynchronous sequence of values of a given property determined by
   /// access tracking and mutation.
   ///
   /// for await name in car.observeValues(for: \.name) {
   ///   print(name)
   /// }
  @available(SwiftStdlib 9999, *)
  public func observeValues<Member: Sendable>(
     for keyPath: KeyPath<Self, Member>
  ) -> some AsyncSequence<Member, Never> {
    ObservedValues(observable: self, keyPath: keyPath)
  }
}

@available(SwiftStdlib 9999, *)
struct ObservedValues<O: Observable & AnyObject, Element> {
  let access: () -> Element?
  
  init(observable: O, keyPath: KeyPath<O, Element>) {
    access = { [weak observable, keyPath] in
      guard let observable else { return nil }
      return observable[keyPath: keyPath]
    }
  }
}

@available(SwiftStdlib 9999, *)
extension ObservedValues: AsyncSequence {
  typealias Failure = Never
  
  struct Iterator: AsyncIteratorProtocol {
    enum State {
      case idle
      case pending(Element?)
      case iterating(UnsafeContinuation<Element?, Never>)
      case terminal
      
      static func cancel(_ state: _ManagedCriticalState<State>) {
        let resumption = state.withCriticalRegion { state -> Resumption? in
          switch state {
          case .idle:
            state = .terminal
            return nil
          case .pending:
            state = .terminal
            return nil
          case .iterating(let continuation):
            state = .terminal
            return Resumption(element: nil, continuation: continuation)
          case .terminal:
            return nil
          }
        }
        resumption?.resume()
      }
    }
    
    final class Sentry {
      let state: _ManagedCriticalState<State>
      
      init(state: _ManagedCriticalState<State>) {
        self.state = state
      }
      
      deinit {
        State.cancel(state)
      }
    }
    
    let access: () -> Element?
    
    let state: _ManagedCriticalState<State>
    var token: ObservationTracking
    
    struct Resumption {
      var element: Element?
      var continuation: UnsafeContinuation<Element?, Never>
      
      func resume() {
        continuation.resume(returning: element)
      }
    }
    
    init(_ access: @escaping () -> Element?) {
      self.access = access
      let state = _ManagedCriticalState<State>(.idle)
      let sentry = Sentry(state: state)
      let (first, token) = withObservationTracking(access, didSet: { tracking in
        let newValue = access()
        let resumption = sentry.state.withCriticalRegion { state -> Resumption? in
          switch state {
          case .idle:
            state = .pending(newValue)
            return nil
          case .pending:
            state = .pending(newValue)
            return nil
          case .iterating(let continuation):
            state = .idle
            return Resumption(element: newValue, continuation: continuation)
          case .terminal:
            return nil
          }
        }
        resumption?.resume()
      })
      let resumption = state.withCriticalRegion { state -> Resumption? in
        switch state {
        case .idle:
          state = .pending(first)
          return nil
        case .pending:
          state = .pending(first)
          return nil
        case .iterating(let continuation):
          state = .idle
          return Resumption(element: first, continuation: continuation)
        case .terminal:
          return nil
        }
      }
      resumption?.resume()
      self.state = state
      self.token = token
    }
    
    func next() async -> Element? {
      await withTaskCancellationHandler {
        return await withUnsafeContinuation { continuation in
          let resumption = state.withCriticalRegion { state -> Resumption? in
            switch state {
            case .idle:
              state = .iterating(continuation)
              return nil
            case .pending(let element):
              state = .idle
              return Resumption(element: element, continuation: continuation)
            case .iterating:
              fatalError()
            case .terminal:
              return Resumption(element: nil, continuation: continuation)
            }
          }
          resumption?.resume()
        }
      } onCancel: {
        State.cancel(state)
        token.cancel()
      }
    }
  }
  
  func makeAsyncIterator() -> Iterator {
    Iterator(access)
  }
}
