// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple) | %FileCheck %s
// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple -O) | %FileCheck %s

// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

protocol Marked {
  func announce()
}
extension Marked {
  func announce() { print("\(Self.self) is Marked") }
}

struct ConcreteWrapper<Wrapped> {}
extension ConcreteWrapper: Marked where Wrapped: Marked {}

struct Hello<T: ~Copyable> {}
extension Hello: Marked {}

func makeWrapper<P>(wrapping _: P.Type) -> some Marked {
    ConcreteWrapper<Hello<P>>()
}

protocol StringConstructable: ~Copyable {
  init(_ s: String)
}
protocol Keeper<Message> {
  associatedtype Message: ~Copyable & StringConstructable
  func getMessage() -> Message
}
struct ConcreteKeeper<Message: ~Copyable & StringConstructable>: Keeper {
  func getMessage() -> Message { return Message("ConcreteKeeper") }
}
struct UniqString: ~Copyable, StringConstructable {
  let str: String
  init(_ str: String) { self.str = str + " as UniqString!" }
}

func makeKeeper<M: ~Copyable & StringConstructable>(with _: M.Type) -> some Keeper<M> {
  return ConcreteKeeper<M>()
}

do {
  let markedVal = makeWrapper(wrapping: String.self)
  markedVal.announce() // CHECK: ConcreteWrapper<Hello<String>> is Marked

  let keeper = makeKeeper(with: UniqString.self)
  print(keeper.getMessage().str) // CHECK: ConcreteKeeper as UniqString!
}
