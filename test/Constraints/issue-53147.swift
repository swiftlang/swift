// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/53147

struct Parser<A> {
  let run: (inout Substring) -> A?

  func map<B>(_ f: @escaping (A) -> B) -> Parser<B> {
    return Parser<B> { input in
      self.run(&input).map(f)
    }
  }
}

let char = Parser<Character> { str in
  guard let match = str.first else { return nil }
  str.removeFirst()
  return match
}

let northSouth = char.map {
  $0 == "N" // expected-warning {{double quotes deprecated in favour of single quotes to express 'Character'}}
      ? 1.0
      : $0 == "S" ? -1 : nil // Ok // expected-warning {{double quotes deprecated in favour of single quotes to express 'Character'}}
}
