// RUN: %target-typecheck-verify-swift

// rdar://119055010 - greedy key path type assignment breaks keypath-to-function conversion

protocol Executable {}

final class Wrapper<Value> {
  func update<Return>(_ work: (inout Value) throws -> Return) rethrows -> Return {
    fatalError()
  }
}

enum Lookup<Value> {
  func flatMap<T>(_ transform: (Value) throws -> Lookup<T>) rethrows -> Lookup<T> { fatalError() }
}

protocol Entry {
}

extension Entry {
  var executable: Lookup<any Executable> {
    fatalError()
  }
}

func lookup() -> Lookup<any Entry> {
  fatalError()
}

struct Test {
  struct Data {
  }

  let value = Wrapper<Data>()

  func run() -> Lookup<any Executable> {
    value.update { data in
      let _ = 42
      return lookup()
    }
    .flatMap(\.executable)
  }
}
