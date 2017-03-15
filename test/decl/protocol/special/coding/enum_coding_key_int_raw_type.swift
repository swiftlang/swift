// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

enum IntKey : Int, CodingKey {
    case a = 3
    case b // Implicitly 4
    case c = 1
}

for (val, str, idx) in [(IntKey.a, "a", 3), (.b, "b", 4), (.c, "c", 1)] {
    // Keys with a raw type of Int should get a stringValue which matches the
    // case name.
    guard val.stringValue == str else { fatalError() }
    guard IntKey(stringValue: str) == val else { fatalError() }

    // Keys with a raw type of Int should get an intValue based on their
    // rawValue.
    guard val.intValue == idx else { fatalError() }
    guard IntKey(intValue: idx) == val else { fatalError() }
}

enum PartialIntKey : Int, CodingKey {
    case a = 3
    case b // Implicitly 4
    case c = 1

    var intValue: Int? {
        return self.rawValue + 1
    }

    var stringValue: String? {
        switch self {
        case .a: return "A"
        case .b: return "B"
        case .c: return "C"
        }
    }
}

for (val, str, idx) in [(PartialIntKey.a, "a", 3), (.b, "b", 4), (.c, "c", 1)] {
    guard val.stringValue == str.uppercased() else { fatalError() }
    guard val.intValue == idx + 1 else { fatalError() }

    // Keys which define some methods should still get derived conformance
    // to the others.
    guard PartialIntKey(stringValue: str) == val else { fatalError() }
    guard PartialIntKey(intValue: idx) == val else { fatalError() }
}
