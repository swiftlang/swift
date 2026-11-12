// rdar://182430935 (https://github.com/swiftlang/swift/issues/90733)
// RUN: %target-typecheck-verify-swift

struct Column<Value> {}
struct Columns {
  let name = Column<String>()
  let raw: Column<Int>? = nil
  var rowid: Int { 0 }
}
@dynamicMemberLookup
struct Path<Root> {
  subscript<Member>(dynamicMember keyPath: KeyPath<Root, Column<Member>>) -> Path<Member> { .init() }
}

func extract<Member>(_ path: KeyPath<Path<Columns>, Path<Member>>) {}
func test1() {
  extract(\.rowid) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func test2(outputs: Path<Columns>) {
  if let target = outputs.raw { _ = target } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
