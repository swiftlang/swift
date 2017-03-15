// REQUIRES: objc_interop

import Foundation

enum NoRawTypeKey : CodingKey {
    case a, b, c
}

enum StringKey : String, CodingKey {
    case a = "A", b, c = "Foo"
}

enum IntKey : Int, CodingKey {
    case a = 3, b, c = 1
}
