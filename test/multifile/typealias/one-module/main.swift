// RUN: %empty-directory(%t)

// RUN: %target-build-swift %S/main.swift %S/Inputs/library.swift
// RUN: %target-build-swift -g %S/main.swift %S/Inputs/library.swift

func testFunction<T>(withCompletion completion: (Result<T, Error>) -> Void) { }
testFunction { (result: GenericResult<Int>) in }

extension Rdar46103190 {
  public typealias AnotherAlias = Self.Alias
  public typealias StringMap = Map<String>
}

typealias Rdar46103190Alias<R: Rdar46103190> = R.Map<String>

struct Rdar46103190Impl: Rdar46103190 {}

func test46103190() {
  let _: String = Rdar46103190Impl.AnotherAlias()
  let _: [String: Rdar46103190Impl] = Rdar46103190Impl.StringMap()
  let _: [String: Rdar46103190Impl] = Rdar46103190Alias()
}
