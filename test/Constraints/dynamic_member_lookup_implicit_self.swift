// RUN: %target-typecheck-verify-swift

// An unqualified reference to a key path based dynamic member inside of a
// `@dynamicMemberLookup` type is resolved through implicit 'self', the same
// as an explicit 'self.<member>'.

struct Color {
  let red: Double // expected-note {{did you mean 'red'?}}
  let green: Double
  let blue: Double // expected-note 2 {{'blue' declared here}}
}

let blue = "global"

@dynamicMemberLookup
struct Lens {
  var color: Color

  subscript<T>(dynamicMember keyPath: KeyPath<Color, T>) -> T {
    color[keyPath: keyPath]
  }

  func implicitSelf() -> Double {
    return red + self.green
  }

  func inNonEscapingClosure() -> [Double] {
    return [1, 2].map { $0 * red }
  }

  func localWins() -> Int {
    let red = 1
    return red
  }

  func globalWins() -> String {
    return blue
  }

  func notAMemberOfRoot() {
    _ = blah // expected-error {{value of type 'Lens' has no dynamic member 'blah' using key path from root type 'Color'; did you mean 'blue'?}}
  }
}

// https://github.com/swiftlang/swift/issues/57282
enum TestNamespace {
  var property: Int { 0 }
}

@dynamicMemberLookup
class Settable {
  subscript<T>(dynamicMember keyPath: KeyPath<TestNamespace, T>) -> T {
    get { fatalError() }
    set { fatalError() }
  }

  func test() {
    property = 42
    self.property = 42
  }
}

// https://github.com/swiftlang/swift/issues/54477
protocol TestProtocol {
  var str1: String { get }
}

@dynamicMemberLookup
class GenericBase<ProtocolType> {
  let proto: ProtocolType

  init(proto: ProtocolType) {
    self.proto = proto
  }

  subscript<T>(dynamicMember keyPath: KeyPath<ProtocolType, T>) -> T {
    return proto[keyPath: keyPath]
  }
}

final class Derived: GenericBase<any TestProtocol> {
  func test() -> String {
    return str1
  }
}

// https://github.com/swiftlang/swift/issues/60589
@dynamicMemberLookup
struct Prop {
  static subscript<T>(dynamicMember member: KeyPath<Color, T>) -> Self {
    Prop()
  }

  static func staticContext() -> Prop {
    return red
  }

  func instanceContext() {
    // Same as 'self.red'.
    _ = red // expected-error {{static member 'red' cannot be used on instance of type 'Prop'}}
  }
}

// A string based subscript accepts any name, so an unqualified name is not
// turned into a dynamic member reference.
@dynamicMemberLookup
struct Stringly {
  subscript(dynamicMember name: String) -> Int { 0 }

  func test() {
    _ = self.anything
    _ = anything // expected-error {{cannot find 'anything' in scope}}
  }
}

@dynamicMemberLookup
struct Mixed {
  var color: Color

  subscript<T>(dynamicMember keyPath: KeyPath<Color, T>) -> T {
    color[keyPath: keyPath]
  }

  subscript(dynamicMember name: String) -> Int { 0 }

  func test() {
    _ = self.red
    _ = red // expected-error {{cannot find 'red' in scope}}
  }
}
