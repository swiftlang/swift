// RUN: %target-typecheck-verify-swift -solver-scope-threshold=500

struct C {
  let c: [C] = []
}

// The overloads of f() and g() are the same, except they are ordered differently.

extension Sequence {
  func f<T: Sequence>(_: (Element) -> T) -> [T.Element] {
    fatalError()
  }

  func f<T>(_: (Element) -> T?) -> [T] {
    fatalError()
  }
}

extension Sequence {
  func g<T>(_: (Element) -> T?) -> [T] {
    fatalError()
  }

  func g<T: Sequence>(_: (Element) -> T) -> [T.Element] {
    fatalError()
  }
}

func ff(_ c: [C]) -> [C] {
  return c
    .f { $0.c }
    .f { $0.c }
    .f { $0.c }
    .f { $0.c }
    .f { $0.c }
}

func gg(_ c: [C]) -> [C] {
  // expected-error@+1 {{reasonable time}}
  return c
    .g { $0.c }
    .g { $0.c }
    .g { $0.c }
    .g { $0.c }
    .g { $0.c }
}

