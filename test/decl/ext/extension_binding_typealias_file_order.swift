// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Make sure the order of files doesn't affect which conformances are found.
// RUN: %target-swift-frontend -typecheck -verify %t/a.swift %t/b.swift
// RUN: %target-swift-frontend -typecheck -verify %t/b.swift %t/a.swift

// The same with a.swift as the only primary file, as in an incremental build.
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/a.swift %t/b.swift
// RUN: %target-swift-frontend -typecheck -verify %t/b.swift -primary-file %t/a.swift

//--- a.swift
struct S: A.P { typealias T = Int }
extension S.T {}

extension B { struct Nested: B.P { typealias T = Int } }
extension B.Nested.T {}

// Inaccessible protocols stay inaccessible.
struct H: A.Hidden { typealias T = Int } // expected-error {{'Hidden' is inaccessible due to 'fileprivate' protection level}}
extension H.T {}

func testConformance() {
  let _: any A.P = S()
  let _: any B.P = B.Nested()
}

//--- b.swift
enum A {}
extension A { protocol P {} }
extension A { fileprivate protocol Hidden {} } // expected-note {{type declared here}}

enum B {}
extension B { protocol P {} }
