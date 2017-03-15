// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

import Foundation

// Enums where CodingKey conformance is added in extensions should still derive
// conformance.
enum NoRawTypeKey {
    case a, b, c
}

extension NoRawTypeKey : CodingKey {}

let _ = NoRawTypeKey.a.stringValue
let _ = NoRawTypeKey(stringValue: "a")
let _ = NoRawTypeKey.a.intValue
let _ = NoRawTypeKey(intValue: 0)

enum StringKey : String {
    case a = "A", b, c = "Foo"
}

extension StringKey : CodingKey {}

let _ = StringKey.a.stringValue
let _ = StringKey(stringValue: "A")
let _ = StringKey.a.intValue
let _ = StringKey(intValue: 0)

enum IntKey : Int {
    case a = 3, b, c = 1
}

extension IntKey : CodingKey {}

let _ = IntKey.a.stringValue
let _ = IntKey(stringValue: "a")
let _ = IntKey.a.intValue
let _ = IntKey(intValue: 3)
