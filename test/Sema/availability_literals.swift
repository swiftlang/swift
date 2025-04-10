// RUN: %target-typecheck-verify-swift -swift-version 5

// REQUIRES: OS=macosx

// https://github.com/apple/swift/issues/61564
// ExpressibleByStringLiteral
struct SLD {}
@available(*, deprecated)
extension SLD: ExpressibleByStringLiteral {
    init(stringLiteral value: StringLiteralType) {}
}

let _ = SLD(stringLiteral: "") // expected-warning{{'init(stringLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}
let _: SLD = "" // expected-warning{{'init(stringLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}


struct SLU {}
@available(macOS 100, *)
extension SLU: ExpressibleByStringLiteral {
    init(stringLiteral value: StringLiteralType) {}
}

let _ = SLU(stringLiteral: "") // expected-error{{'init(stringLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}
let _: SLU = "" // expected-error{{'init(stringLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}

// ExpressibleByIntegerLiteral
struct ILD {}
@available(*, deprecated)
extension ILD: ExpressibleByIntegerLiteral {
  init(integerLiteral value: IntegerLiteralType) {}
}

let _ = ILD(integerLiteral: 1) // expected-warning{{'init(integerLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}
let _: ILD = 1 // expected-warning{{'init(integerLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}

struct ILU {}

@available(macOS 100, *)
extension ILU: ExpressibleByIntegerLiteral {
  init(integerLiteral value: IntegerLiteralType) {}
}

let _ = ILU(integerLiteral: 1) // expected-error{{'init(integerLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}
let _: ILU = 1 // expected-error{{'init(integerLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}

// ExpressibleByNilLiteral
struct NLD {}

@available(*, deprecated)
extension NLD: ExpressibleByNilLiteral {
  init(nilLiteral: ()) {}
}

let _: NLD = .init(nilLiteral: ()) // expected-warning{{'init(nilLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}
let _: NLD = nil // expected-warning{{'init(nilLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}

struct NLU {}

@available(macOS 100, *)
extension NLU: ExpressibleByNilLiteral {
  init(nilLiteral: ()) {}
}

let _: NLU = .init(nilLiteral: ()) // expected-error{{'init(nilLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}
let _: NLU = nil // expected-error{{'init(nilLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}

// ExpressibleByBooleanLiteral
struct BLD {}
@available(*, deprecated)
extension BLD: ExpressibleByBooleanLiteral {
  init(booleanLiteral value: BooleanLiteralType) {}
}
let _: BLD = .init(booleanLiteral: false) // expected-warning{{'init(booleanLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}
let _: BLD = false // expected-warning{{'init(booleanLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}

struct BLU {}
@available(macOS 100, *)
extension BLU: ExpressibleByBooleanLiteral {
  init(booleanLiteral value: BooleanLiteralType) {}
}
let _: BLU = .init(booleanLiteral: false) // expected-error{{'init(booleanLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}
let _: BLU = false // expected-error{{'init(booleanLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}

// ExpressibleByFloatLiteral
struct FLD {}
@available(*, deprecated)
extension FLD: ExpressibleByFloatLiteral {
  init(floatLiteral value: FloatLiteralType) {}
}
let _: FLD = .init(floatLiteral: 0.1) // expected-warning{{'init(floatLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}
let _: FLD = 0.1 // expected-warning{{'init(floatLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}

struct FLU {}
@available(macOS 100, *)
extension FLU: ExpressibleByFloatLiteral {
  init(floatLiteral value: FloatLiteralType) {}
}
let _: FLU = .init(floatLiteral: 0.1) // expected-error{{'init(floatLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}
let _: FLU = 0.1 // expected-error{{'init(floatLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}

// ExpressibleByArrayLiteral
struct ALD {}
@available(*, deprecated)
extension ALD: ExpressibleByArrayLiteral {
  init(arrayLiteral elements: Int...) {}
}
let _: ALD = .init(arrayLiteral: 1) // expected-warning{{'init(arrayLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}
let _: ALD = [1] // expected-warning{{'init(arrayLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}

struct ALU {}
@available(macOS 100, *)
extension ALU: ExpressibleByArrayLiteral {
  init(arrayLiteral elements: Int...) {}
}
let _: ALU = .init(arrayLiteral: 1) // expected-error{{'init(arrayLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}
let _: ALU = [1] // expected-error{{'init(arrayLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}

// ExpressibleByDictionaryLiteral
struct DLD {}
@available(*, deprecated)
extension DLD: ExpressibleByDictionaryLiteral {
  init(dictionaryLiteral elements: (Int, Int)...) {}
}
let _: DLD = .init(dictionaryLiteral: (1,1)) // expected-warning{{'init(dictionaryLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}
let _: DLD = [1: 1] // expected-warning{{'init(dictionaryLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}

struct DLU {}
@available(macOS 100, *)
extension DLU: ExpressibleByDictionaryLiteral {
  init(dictionaryLiteral elements: (Int, Int)...) {}
}
let _: DLU = .init(dictionaryLiteral: (1,1)) // expected-error{{'init(dictionaryLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}
let _: DLU = [1: 1] // expected-error{{'init(dictionaryLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}

// ExpressibleByUnicodeScalarLiteral
struct USLD {}
@available(*, deprecated)
extension USLD: ExpressibleByUnicodeScalarLiteral {
  typealias UnicodeScalarLiteralType = Character
  init(unicodeScalarLiteral value: UnicodeScalarLiteralType) {}
}
let _: USLD = .init(unicodeScalarLiteral: "a") // expected-warning{{'init(unicodeScalarLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}
let _: USLD = "a" // expected-warning{{'init(unicodeScalarLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}

struct USLU {}
@available(macOS 100, *)
extension USLU: ExpressibleByUnicodeScalarLiteral {
  typealias UnicodeScalarLiteralType = Character
  init(unicodeScalarLiteral value: UnicodeScalarLiteralType) {}
}
let _: USLU = .init(unicodeScalarLiteral: "a") // expected-error{{'init(unicodeScalarLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}
let _: USLU = "a" // expected-error{{'init(unicodeScalarLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}

//ExpressibleByExtendedGraphemeClusterLiteral
struct GCLD {}
@available(*, deprecated)
extension GCLD: ExpressibleByExtendedGraphemeClusterLiteral {
  init(extendedGraphemeClusterLiteral value: Character) {}
}
let _: GCLD = .init(extendedGraphemeClusterLiteral: "🇧🇷") // expected-warning{{'init(extendedGraphemeClusterLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}
let _: GCLD = "🇧🇷" // expected-warning{{'init(extendedGraphemeClusterLiteral:)' is deprecated}}{{documentation-file=deprecated-declaration}}

struct GCLU {}
@available(macOS 100, *)
extension GCLU: ExpressibleByExtendedGraphemeClusterLiteral {
  init(extendedGraphemeClusterLiteral value: Character) {}
}
let _: GCLU = .init(extendedGraphemeClusterLiteral: "🇧🇷") // expected-error{{'init(extendedGraphemeClusterLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}
let _: GCLU = "🇧🇷" // expected-error{{'init(extendedGraphemeClusterLiteral:)' is only available in macOS 100 or newer}} expected-note{{add 'if #available' version check}}
