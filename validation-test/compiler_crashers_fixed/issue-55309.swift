// RUN: %target-swift-frontend -typecheck %s -verify

// https://github.com/apple/swift/issues/55309

enum E {
    case bar
}

@dynamicMemberLookup
struct S {
    subscript(dynamicMember key: KeyPath<E.Type, E>) -> Bool { true }
}


let s = S()
let e = s.bar // expected-error {{dynamic key path member lookup cannot refer to enum case 'bar'}}
