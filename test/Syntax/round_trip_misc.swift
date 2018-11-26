// RUN: %round-trip-syntax-test --swift-syntax-test %swift-syntax-test --file %s

class C {
  // Erroneous typealias decl.
  typealias Inner: Foo = Int

  // Implict accessor with attribute at the top of its body.
  var x: Int {
    @objc
    func f() {}
  }
}

// Orphan '}' at top level
}

// Compound name.
foo(x:y:)()

// Type identifier with erroneous component.
let a: Int.)

// Type with unknown attribute followed by parentheses.
typealias b = @foobar() -> Void
typealias c = @foobar(a) () -> Void

// keypath expressions.
let d = \.foo
let e = \.[1]
let f = \.?.bar

let optionalArray: [Int]?
