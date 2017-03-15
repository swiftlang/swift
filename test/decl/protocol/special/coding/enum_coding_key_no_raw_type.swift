// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

enum ImplicitKey : CodingKey {
    case a, b, c
}

for (index, (val, str)) in [(ImplicitKey.a, "a"), (.b, "b"), (.c, "c")].enumerated() {
    // Keys with no raw type should get a stringValue which matches the case
    // name.
    guard val.stringValue == str else { fatalError() }
    guard ImplicitKey(stringValue: str) == val else { fatalError() }

    // They should not have an intValue.
    guard val.intValue == nil else { fatalError() }
    guard ImplicitKey(intValue: index) == nil else { fatalError() }
}

enum PartialImplicitKey : CodingKey {
    case a, b, c

    var intValue: Int? {
        switch self {
        case .a: return 1
        case .b: return 2
        case .c: return 3
        }
    }

    var stringValue: String? {
        switch self {
        case .a: return "A"
        case .b: return "B"
        case .c: return "C"
        }
    }
}

for (val, str, idx) in [(PartialImplicitKey.a, "a", 1), (.b, "b", 2), (.c, "c", 3)] {
    guard val.stringValue == str.uppercased() else { fatalError() }
    guard val.intValue == idx else { fatalError() }

    // Keys which define some methods should still get derived conformance
    // to the others.
    guard PartialImplicitKey(stringValue: str) == val else { fatalError() }
    guard PartialImplicitKey(intValue: idx) == nil else { fatalError() }
}
