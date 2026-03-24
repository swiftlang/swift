// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated
// RUN: not --crash %target-typecheck-verify-swift -verify-ignore-unrelated -DSALVAGE

// The next two sets of examples cause difficulties because our
// subtype lattice is not distributive. We cannot always compute
// an accurate upper bound, because of existential types; if
// we pick the wrong upper bound too early, certain expressions
// will fail.

// Originally from rdar://35088384:
//
// This used to crash in Xcode 9 GM, and fails with a diagnostic in more
// recent swift-4.0-branch builds, because we incorrectly infer the type
// of the array literal as [Any].
do {
  protocol Command {}

  struct Undo: Command {}
  struct Cut: Command {}
  struct Copy: Command {}
  struct Paste: Command {}

  let _: Array<any Command> = [Cut(), Copy(), Paste()]
  let _: Array<(any Command)?> = [Cut(), Copy(), Paste()]
  let _: Array<any Command.Type> = [Cut.self, Copy.self, Paste.self]
  let _: Array<(any Command.Type)?> = [Cut.self, Copy.self, Paste.self]

  let _ = Array<any Command>([Cut(), Copy(), Paste()])
  // expected-error@-1 {{no exact matches in call to initializer}}
  let _ = Array<(any Command)?>([Cut(), Copy(), Paste()])
  // expected-error@-1 {{cannot convert value of type '[Any]' to expected argument type '[(any Command)?]'}}
  // expected-note@-2 {{arguments to generic parameter 'Element' ('Any' and '(any Command)?') are expected to be equal}}
  let _ = Array<any Command.Type>([Cut.self, Copy.self, Paste.self])
  let _ = Array<(any Command.Type)?>([Cut.self, Copy.self, Paste.self])

  var commands1: [any Command] = [Undo(), Cut()]
  commands1.append(contentsOf: [Copy(), Paste()])
  // expected-error@-1 {{no exact matches in call to instance method 'append'}}

  var commands2: [any Command.Type] = [Undo.self, Cut.self]
  commands2.append(contentsOf: [Copy.self, Paste.self])

  var commands3: [(any Command)?] = [Undo(), Cut()]
  commands3.append(contentsOf: [Copy(), Paste()])
  // expected-error@-1 {{cannot convert value of type '[Any]' to expected argument type '[(any Command)?]'}}
  // expected-note@-2 {{arguments to generic parameter 'Element' ('Any' and '(any Command)?') are expected to be equal}}

  var commands4: [(any Command.Type)?] = [Undo.self, Cut.self]
  commands4.append(contentsOf: [Copy.self, Paste.self])

  func perform1<S: Sequence>(_: S) where S.Element == Any {}
  perform1([Undo(), Cut(), Copy()])

  func perform2<S: Sequence>(_: S) where S.Element == Any.Type {}
  perform2([Undo.self, Cut.self, Copy.self])

  func perform3<S: Sequence>(_: S) where S.Element == Any? {}
  perform3([Undo(), Cut(), Copy()])

  func perform4<S: Sequence>(_: S) where S.Element == Any.Type? {}
  perform4([Undo.self, Cut.self, Copy.self])
}

// rdar://problem/38159133
// https://github.com/apple/swift/issues/49673
// Swift 4.1 Xcode 9.3b4 regression

do {
  protocol Command {}

  class Super {}
  class A: Super, Command {}
  class B: Super, Command {}

  func rdar38159133(a: A, b: B, aOpt: A?, bOpt: B?) {
    let _ = Array<any Command>([a, b])
    let _: [any Command] = [a, b]
    let _: [any Command] = Array([a, b])
    let _: [any Command] = [a, b].filter { _ in true }
    let _: [any Command] = [aOpt, bOpt].compactMap { $0 }

    // Some of these might be hard to resolve, but we should produce better diagnostics.

    let _: [any Command] = [aOpt, bOpt].filter { $0 != nil }
    // expected-error@-1 {{no exact matches in call to instance method 'filter'}}
    let _: [any Command] = [a, b].map { $0 }
    // expected-error@-1 {{failed to produce diagnostic for expression}}
    let _: [any Command] = [a, b].flatMap { [$0] }
    // expected-error@-1 {{cannot convert value of type 'Super' to expected element type 'any Command'}}

    #if SALVAGE
    let _: [any Command] = [[a], [b]].flatMap { $0 }
    #endif
  }
}

do {
  // This works!
  let _: [ObjectIdentifier: Bool] = .init(uniqueKeysWithValues: [
    Optional<String>.self, Int.self
  ].map { (ObjectIdentifier($0), false) })

  // FIXME: This is broken
  let _: [ObjectIdentifier: Bool] = .init(uniqueKeysWithValues: [
    String.self, Optional<String>.self, Array<String>.self
  ].map { (ObjectIdentifier($0), false) })
  // expected-error@-1 {{argument type 'Any' expected to be an instance of a class or class-constrained type}}

  // This works too!
  let _: [ObjectIdentifier: Bool] = .init(uniqueKeysWithValues: [
    String.self, Optional<String>.self, Array<String>.self, Int.self
  ].map { (ObjectIdentifier($0), false) })
}

// This expression first appeared in test/embedded/dict-init.swift.
// Test some variations.
do {
  let _ = Dictionary(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Int, String>(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Double, String>(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Int, StaticString>(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Double, StaticString>(uniqueKeysWithValues: [(10, "hello"), (20, "world")])

  let _ = Dictionary.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Int, String>.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Double, String>.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Int, StaticString>.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _ = Dictionary<Double, StaticString>.init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])

  let _: Dictionary = .init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _: Dictionary<Int, String> = .init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _: Dictionary<Double, String> = .init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _: Dictionary<Int, StaticString> = .init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
  let _: Dictionary<Double, StaticString> = .init(uniqueKeysWithValues: [(10, "hello"), (20, "world")])
}

// Tests for a special form of inference where we have both a
// subtype and a supertype binding for a type variable, and the
// subtype binding contains a type variable but the supertype
// does not. We propagate the more concrete type from the
// supertype binding to the subtype binding in this case.
//
// See BindingSet::addBinding() for details.

// FIXME: This needs more tests.

do {
  func f<R>(fn: () -> [R]) -> [R] { [] }

  // Requires collection upcast from Array<(key: String, value: String)> to `Array<(String, String)>`
  func g(v: [String: String]) {
    let _: [(String, String)] = f { return Array(v) } + v
    let _: [(String, String)] = v + f { return Array(v) }
    let _: [(String, String)] = f { return Array(v) } + Array(v)
    let _: [(String, String)] = Array(v) + f { return Array(v) }
    let _: [(String, String)] = v + Array(v)
    let _: [(String, String)] = Array(v) + v
    let _: [(String, String)] = Array(v) + Array(v)
  }
}

protocol P {}

extension String: P {}
extension Int: P {}
extension Optional: P where Wrapped: P {}

do {
  class C {
    var string = ""
    var integer = 0
    var data = Data()
    var optData: Data? = nil
    var optOptData: Data?? = nil
    var dictionary: [AnyHashable: Any]? = nil

    static func _data(_: [AnyHashable: Any]?) -> Data {
      return Data()
    }

    static func _optData(_: [AnyHashable: Any]?) -> Data? {
      return nil
    }
  }

  struct Data: P {}

  func f1<T: Collection, U: Collection>(_: T)
    where T.Element == U, U.Element == any P {}
  // expected-note@-2 5{{where 'U.Element' = 'Any'}}

  func test1(_ elts: [C]) {
    f1(elts.map { c in [c.string] })

    f1(elts.map { c in [c.string, c.integer] })
    // expected-error@-1 {{local function 'f1' requires the types 'Any' and 'any P' be equivalent}}

    f1(elts.map { c in [c.string, c.integer, c.data] })
    // expected-error@-1 {{local function 'f1' requires the types 'Any' and 'any P' be equivalent}}

    f1(elts.map { c in [c.string, c.integer, c.optData] })
    // expected-error@-1 {{local function 'f1' requires the types 'Any' and 'any P' be equivalent}}

    f1(elts.map { c in [c.string, c.integer, c.optOptData] })

    f1(elts.map { c in [c.string, c.integer, c.data, c.optData] })
    // expected-error@-1 {{local function 'f1' requires the types 'Any' and 'any P' be equivalent}}

    f1(elts.map { c in [c.string, c.integer, c.data, c.optData, c.optOptData] })

    f1(elts.map { c in [c.string, c.integer, c.dictionary.map { C._data($0) }] })
    // expected-error@-1 {{local function 'f1' requires the types 'Any' and 'any P' be equivalent}}

    f1(elts.map { c in [c.string, c.integer, c.dictionary.map { C._optData($0) }] })
  }

  struct Literal: ExpressibleByStringInterpolation {
    init(stringInterpolation: Interpolation) {}
    init(stringLiteral: String) {}

    struct Interpolation: StringInterpolationProtocol {
      typealias StringLiteralType = String

      init(literalCapacity: Int, interpolationCount: Int) {}

      func appendLiteral(_: String) {}
      func appendInterpolation<T: Collection, U: Collection>(_: T)
        where T.Element == U, U.Element == any P {}
        // expected-note@-2 5{{where 'U.Element' = 'Any'}}
    }
  }

  func f2(_: Literal) {}

  func test2(_ elts: [C]) {
    f2("\(elts.map { c in [c.string] })")

    f2("\(elts.map { c in [c.string, c.integer] })")
    // expected-error@-1 {{instance method 'appendInterpolation' requires the types 'Any' and 'any P' be equivalent}}

    f2("\(elts.map { c in [c.string, c.integer, c.data] })")
    // expected-error@-1 {{instance method 'appendInterpolation' requires the types 'Any' and 'any P' be equivalent}}

    f2("\(elts.map { c in [c.string, c.integer, c.optData] })")
    // expected-error@-1 {{instance method 'appendInterpolation' requires the types 'Any' and 'any P' be equivalent}}

    f2("\(elts.map { c in [c.string, c.integer, c.optOptData ] })")

    f2("\(elts.map { c in [c.string, c.integer, c.data, c.optData] })")
    // expected-error@-1 {{instance method 'appendInterpolation' requires the types 'Any' and 'any P' be equivalent}}

    f2("\(elts.map { c in [c.string, c.integer, c.data, c.optData, c.optOptData] })")

    f2("\(elts.map { c in [c.string, c.integer, c.dictionary.map { C._data($0) }] })")
    // expected-error@-1 {{instance method 'appendInterpolation' requires the types 'Any' and 'any P' be equivalent}}

    f2("\(elts.map { c in [c.string, c.integer, c.dictionary.map { C._optData($0) }] })")
  }
}

// https://github.com/swiftlang/swift/issues/77003
do {
  struct Task<E> {
    init(_: () -> ()) where E == Never {}
    init(_: () throws -> ()) where E == Error {}
  }

  func f0<T, U>(_: T.Type, _ fn: (T) -> U, _: (U) -> ()) {}

  func f1<T, U>(_: T.Type, _ fn: (T) -> U?, _: (U) -> ()) {}

  func g1(x: Int?.Type) {
      f0(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
      f1(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
  }

  func f2<T, U>(_: T.Type, _ fn: (T) -> (U, U), _: (U) -> ()) {}

  func g2(x: (Int, Int).Type) {
      f0(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
      f2(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
  }

  func f3<T, U>(_: T.Type, _ fn: (T) -> [U], _: (U) -> ()) {}

  func g3(x: [Int].Type) {
      f0(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
      f3(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
  }

  func f4<T, U>(_: T.Type, _ fn: (T) -> [U?], _: (U) -> ()) {}

  func g4(x: [Int?].Type) {
      f0(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
      f4(x, { $0 }, { _ in Task {} })  // expected-warning {{result of 'Task<E>' initializer is unused}}
  }
}

// Regression tests for closure returns defaulting to Void.
// Here, we want to make sure that we don't diagnose ambiguity
// because there are two places where we can convert to Void
// here; in the outermost closure passed to g(), or the closure
// passed to f().
do {
  @discardableResult
  func f<T>(_: () -> T) -> T { fatalError() }

  func g<T>(_: T, _: (T) -> ()) {}

  struct R {
    func m() -> Bool { fatalError() }
  }

  func h(r: R) {
    g(r) { r in
      f {
        r.m()
      }
    }
  }
}

// More elaborate variant of the above.
do {
  @discardableResult
  func f<T>(_: () -> T) -> T { fatalError() }

  @discardableResult
  func f<T>(_: () async -> T) async -> T { fatalError() }

  protocol P {}

  struct R: P {
    @discardableResult func g() async -> String {
       fatalError()
    }
  }

  class C {
    func g<T: P>(_: T, _: (T) async -> ()) {}

    func h(r: R) {
      g(r) { r in
        await f {
          await r.g()
        }
      }
    }
  }
}