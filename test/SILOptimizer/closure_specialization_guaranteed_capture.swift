// RUN: %target-swift-frontend -O -emit-sil %s

// Regression test: closure specialization crashed when an on-stack (non-escaping)
// closure's captured argument has guaranteed ownership (e.g. from a load_borrow
// in a protocol witness thunk), and an existing escaping specialization of the
// same callee — with @owned parameter convention — was reused due to mangling
// collision between on-stack and escaping partial_applys of the same function.
//
// The fix inserts a copy_value in rewriteApply when a guaranteed argument is
// passed to an @owned parameter of the (reused) specialized function.

public protocol Receiver: AnyObject {
  associatedtype State
  func receive(_ state: State)
}

enum Mode: Equatable {
  case selected(Bool)
  case typing
}

struct Input: Equatable {
  var x: Int
  var y: Int
}

struct Output: Equatable {
  final class Box {}
  var mode: Mode
  var box: Box
  var input: Input

  static func == (lhs: Output, rhs: Output) -> Bool {
    lhs.mode == rhs.mode && lhs.box === rhs.box && lhs.input == rhs.input
  }
}

public final class Store {
  public func add<R>(_ state: R.State, _ receiver: R) where R: Receiver {
    receiver.receive(state)
  }
}

final class ConcreteReceiver: Receiver {
  typealias State = (String) -> Output?
  private var id: String
  private var output: Output?

  init(id: String) { self.id = id }

  func receive(_ state: (String) -> Output?) {
    let old = output
    let new = state(id)
    if old == new { return }
    output = new
  }
}

// Coordinator.receive passes an owned closure to child.receive, causing the
// closure specializer to create a specialization of ConcreteReceiver.receive
// with an @owned capture parameter (processed first).
final class Coordinator: Receiver {
  typealias State = Any
  private let child: ConcreteReceiver

  init(child: ConcreteReceiver) { self.child = child }

  func receive(_ state: Any) {
    guard let pair = state as? ((String) -> Output?, (String) -> Output?) else { return }
    child.receive(pair.1)
  }
}

// entry calls Store.add which dispatches through the protocol witness thunk for
// ConcreteReceiver.receive. The witness thunk uses load_borrow, giving the
// captured closure guaranteed ownership. The closure specializer reuses the
// existing @owned specialization and must insert a copy_value.
@inline(never)
func entry(_ state: @escaping (String) -> Output?) {
  let receiver = ConcreteReceiver(id: "id")
  Store().add(state, receiver)
}
