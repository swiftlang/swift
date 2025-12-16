// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/57165

@resultBuilder
struct MyResultBuilder {
  static func buildBlock(_ elements: Int...) -> Int { fatalError() }
}

struct VariableDecl2 {
  init( // expected-note {{found candidate with type '(() -> Int?, Int, () -> Int?) -> VariableDecl2'}}
    paramClosure: () -> Int? = { nil },
    paramInt: Int,
    @MyResultBuilder paramResultBuilder: () -> Int? = { nil }
  ) { fatalError() }

  init( // expected-note {{found candidate with type '(() -> Int?, Int, () -> Int?) -> VariableDecl2'}}
    paramInt: Int,
    paramClosure: () -> Int? = { nil },
    @MyResultBuilder paramResultBuilder: () -> Int? = { nil }
  ) {
    fatalError()
  }
}

let buildable = VariableDecl2(paramInt: 1) { // expected-error {{ambiguous use of 'init'}}
}
