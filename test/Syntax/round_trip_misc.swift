// RUN: %round-trip-syntax-test --swift-syntax-test %swift-syntax-test --file %s

class C {
  // Erroneous typealias decl.
  typealias Inner: Foo = Int

  typealias Alias1 = [Generic<Int


  // Implict accessor with attribute at the top of its body.
  var x: Int {
    @objc
    func f() {}
  }
}
do {
  typealias Alias2 = () -> (a b: [Generic<Int
}
do {
  typealias Alias3 = (a b C, 
}
do {
  typealias Alias3 = () -> @objc func
}
do {
  typealias
}
do {
  typealias Alias = A & B & C.D<>
}
do {
  typealias boo bar = Int
}

// Orphan '}' at top level
}

// Orphan #elseif, #else, #endif at top level.
#elseif foobar
#else
#endif

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
