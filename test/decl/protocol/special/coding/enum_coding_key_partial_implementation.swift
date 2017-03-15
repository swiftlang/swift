// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

import Foundation

// Enums which conform to CodingKey and which should derive conformance should
// get all synthesized methods that they do not implement themselves.
enum NoRawTypeKey1 : CodingKey {
    case a, b, c
}

let _ = NoRawTypeKey1.a.stringValue
let _ = NoRawTypeKey1(stringValue: "a")
let _ = NoRawTypeKey1.a.intValue
let _ = NoRawTypeKey1(intValue: 0)

enum NoRawTypeKey2 : CodingKey {
    case a, b, c
    var stringValue: String? { return nil }
}

let _ = NoRawTypeKey2.a.stringValue
let _ = NoRawTypeKey2(stringValue: "a")
let _ = NoRawTypeKey2.a.intValue
let _ = NoRawTypeKey2(intValue: 0)

enum NoRawTypeKey3 : CodingKey {
    case a, b, c
    var intValue: Int? { return nil }
}

let _ = NoRawTypeKey3.a.stringValue
let _ = NoRawTypeKey3(stringValue: "a")
let _ = NoRawTypeKey3.a.intValue
let _ = NoRawTypeKey3(intValue: 0)

enum NoRawTypeKey4 : CodingKey {
    case a, b, c
    init?(stringValue: String) { return nil }
}

let _ = NoRawTypeKey4.a.stringValue
let _ = NoRawTypeKey4(stringValue: "a")
let _ = NoRawTypeKey4.a.intValue
let _ = NoRawTypeKey4(intValue: 0)

enum NoRawTypeKey5 : CodingKey {
    case a, b, c
    init?(intValue: Int) { return nil }
}

let _ = NoRawTypeKey5.a.stringValue
let _ = NoRawTypeKey5(stringValue: "a")
let _ = NoRawTypeKey5.a.intValue
let _ = NoRawTypeKey5(intValue: 0)

enum NoRawTypeKey6 : CodingKey {
    case a, b, c
    var stringValue: String? { return nil }
    var intValue: Int? { return nil }
}

let _ = NoRawTypeKey6.a.stringValue
let _ = NoRawTypeKey6(stringValue: "a")
let _ = NoRawTypeKey6.a.intValue
let _ = NoRawTypeKey6(intValue: 0)

enum NoRawTypeKey7 : CodingKey {
    case a, b, c
    var stringValue: String? { return nil }
    init?(stringValue: String) { return nil }
}

let _ = NoRawTypeKey7.a.stringValue
let _ = NoRawTypeKey7(stringValue: "a")
let _ = NoRawTypeKey7.a.intValue
let _ = NoRawTypeKey7(intValue: 0)

enum NoRawTypeKey8 : CodingKey {
    case a, b, c
    var stringValue: String? { return nil }
    init?(intValue: Int) { return nil }
}

let _ = NoRawTypeKey8.a.stringValue
let _ = NoRawTypeKey8(stringValue: "a")
let _ = NoRawTypeKey8.a.intValue
let _ = NoRawTypeKey8(intValue: 0)

enum NoRawTypeKey9 : CodingKey {
    case a, b, c
    var intValue: Int? { return nil }
    init?(stringValue: String) { return nil }
}

let _ = NoRawTypeKey9.a.stringValue
let _ = NoRawTypeKey9(stringValue: "a")
let _ = NoRawTypeKey9.a.intValue
let _ = NoRawTypeKey9(intValue: 0)

enum NoRawTypeKey10 : CodingKey {
    case a, b, c
    var intValue: Int? { return nil }
    init?(intValue: Int) { return nil }
}

let _ = NoRawTypeKey10.a.stringValue
let _ = NoRawTypeKey10(stringValue: "a")
let _ = NoRawTypeKey10.a.intValue
let _ = NoRawTypeKey10(intValue: 0)

enum NoRawTypeKey11 : CodingKey {
    case a, b, c
    var stringValue: String? { return nil }
    var intValue: Int? { return nil }
    init?(stringValue: String) { return nil }
}

let _ = NoRawTypeKey11.a.stringValue
let _ = NoRawTypeKey11(stringValue: "a")
let _ = NoRawTypeKey11.a.intValue
let _ = NoRawTypeKey11(intValue: 0)

enum NoRawTypeKey12 : CodingKey {
    case a, b, c
    var stringValue: String? { return nil }
    var intValue: Int? { return nil }
    init?(intValue: Int) { return nil }
}

let _ = NoRawTypeKey12.a.stringValue
let _ = NoRawTypeKey12(stringValue: "a")
let _ = NoRawTypeKey12.a.intValue
let _ = NoRawTypeKey12(intValue: 0)

// Enums which provide implementations for all CodingKey methods should not
// derive conformance (but derived conformance should not interfere with the
// existing methods).
enum NoRawTypeKey13 : CodingKey {
    case a, b, c
    var stringValue: String? { return nil }
    var intValue: Int? { return nil }
    init?(stringValue: String) { return nil }
    init?(intValue: Int) { return nil }
}

let _ = NoRawTypeKey13.a.stringValue
let _ = NoRawTypeKey13(stringValue: "a")
let _ = NoRawTypeKey13.a.intValue
let _ = NoRawTypeKey13(intValue: 0)
