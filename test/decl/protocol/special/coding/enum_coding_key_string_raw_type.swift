// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

enum StringKey : String, CodingKey {
    case a = "A"
    case b // Implicitly "b"
    case c = "Foo"
}

for (index, (val, str)) in [(StringKey.a, "A"), (.b, "b"), (.c, "Foo")].enumerated() {
    // Keys with a raw type of String should get a stringValue based on their
    // rawValue.
    guard val.stringValue == str else { fatalError() }
    guard StringKey(stringValue: str) == val else { fatalError() }

    // They should not have an intValue.
    guard val.intValue == nil else { fatalError() }
    guard StringKey(intValue: index) == nil else { fatalError() }
}

enum PartialStringKey : String, CodingKey {
    case a = "A"
    case b // Implicitly "b"
    case c = "Foo"

    var intValue: Int? {
        switch self {
        case .a: return 1
        case .b: return 2
        case .c: return 3
        }
    }

    var stringValue: String {
        switch self {
        case .a: return "x"
        case .b: return "y"
        case .c: return "z"
        }
    }
}

for (val, realStr, str, idx) in [(PartialStringKey.a, "A", "x", 1), (.b, "b", "y", 2), (.c, "Foo", "z", 3)] {
    guard val.stringValue == str else { fatalError() }
    guard val.intValue == idx else { fatalError() }

    // Keys which define some methods should still get derived conformance
    // to the others.
    guard PartialStringKey(stringValue: realStr) == val else { fatalError() }
    guard PartialStringKey(intValue: idx) == nil else { fatalError() }
}
