// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

struct G<each T>: Sequence {
  typealias Element = Int
  typealias Iterator = [Int].Iterator

  consuming func makeIterator() -> Iterator {
    fatalError()
  }
}

// expected-note@+1 {{in call to function 'foo'}}
func foo<each T>(_: repeat each T) -> G<repeat each T> {
  .init()
}

// expected-error@+2 {{for-in loop requires '(repeat each T) -> G<repeat each T>' to conform to 'Sequence'}}
// expected-error@+1 {{generic parameter 'each T' could not be inferred}}
for a in foo {
  print(a)
}

for a in foo() {
  print(a)
}
