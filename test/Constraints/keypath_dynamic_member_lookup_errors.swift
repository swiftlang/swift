// RUN: %target-typecheck-verify-swift

@dynamicMemberLookup
struct ObservedObject<T> {
  let root: T

  subscript<U>(
    dynamicMember keyPath: ReferenceWritableKeyPath<T, U>
  ) -> U {
    fatalError()
  }
}

protocol P {
  var checked: Bool { get set }
}

struct Test<U: P> {
  var v: ObservedObject<U>

  func test() {
    v.checked
    // expected-error@-1 {{referencing property 'checked' on 'U' requires it to be a class type}}
    // expected-note@-2 {{'subscript(dynamicMember:)' declared in 'ObservedObject<U>' uses 'ReferenceWritableKeyPath' which only supports classes}}
  }
}

// https://github.com/swiftlang/swift/issues/90751 - a member whose type does
// not satisfy the subscript's key path value type should be diagnosed rather
// than crashing with "failed to produce diagnostic".
struct Column<Value> {}

struct Columns {
  var rowid: Int { 0 }
  let age = Column<Int>()
}

@dynamicMemberLookup
struct Path<Value> {
  subscript<Member>(dynamicMember keyPath: KeyPath<Columns, Column<Member>>) -> Path<Member> {
    // expected-note@-1 {{in call to 'subscript(dynamicMember:)'}}
    Path<Member>()
  }
}

func testMemberValueMismatch(p: Path<String>) {
  _ = p.rowid
  // expected-error@-1 {{generic parameter 'Member' could not be inferred}}
  _ = p.age // Ok
}

func extract<Member>(_ path: KeyPath<Path<String>, Path<Member>>) {}
// expected-note@-1 {{in call to function 'extract'}}

func testKeyPathValueMismatch() {
  extract(\.rowid)
  // expected-error@-1 {{generic parameter 'Member' could not be inferred}}
}

@dynamicMemberLookup
struct ConcretePath {
  subscript(dynamicMember keyPath: KeyPath<Columns, Column<Int>>) -> Bool { true }
}

func testConcreteValueMismatch(p: ConcretePath) {
  _ = p.rowid
  // expected-error@-1 {{key path value type 'Int' cannot be converted to contextual type 'Column<Int>'}}
}
