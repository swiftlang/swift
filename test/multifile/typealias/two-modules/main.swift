// RUN: %empty-directory(%t)

// RUN: %empty-directory(%t/linker)
// RUN: %target-build-swift -emit-module -c %S/Inputs/library.swift -o %t/linker/library.o
// RUN: %target-build-swift -emit-library -c %S/Inputs/library.swift -o %t/linker/library.o
// RUN: %target-build-swift %S/main.swift %t/linker/library.o -I %t/linker/ -L %t/linker/ -o %t/linker/main

// RUN: %target-build-swift -g -emit-module -c %S/Inputs/library.swift -o %t/linker/library.o
// RUN: %target-build-swift -g -emit-library -c %S/Inputs/library.swift -o %t/linker/library.o
// RUN: %target-build-swift -g %S/main.swift %t/linker/library.o -I %t/linker/ -L %t/linker/ -o %t/linker/main

import library

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
