// 01_basic_types.swift - Basic types and literals

// ========== BASIC TYPES & LITERALS ==========
// Numbers
let int: Int = 42
let uint: UInt = 42
let int8: Int8 = 127
let int16: Int16 = 32767
let int32: Int32 = 2147483647
let int64: Int64 = 9223372036854775807
let uint8: UInt8 = 255
let uint16: UInt16 = 65535
let uint32: UInt32 = 4294967295
let uint64: UInt64 = 18446744073709551615

let float: Float = 3.14
let double: Double = 3.14159265359
let hexInt = 0xFF
let binaryInt = 0b1010
let octalInt = 0o777
let scientificDouble = 1.23e4
let hexDouble = 0x1.3p2

// Strings
let string: String = "Hello, World!"
let char: Character = "A"
let unicodeScalar: UnicodeScalar = "🚀"
let extendedGraphemeCluster = "café"

// Booleans
let trueValue: Bool = true
let falseValue: Bool = false

// Raw string literals
let rawString = #"Line 1\nLine 2"#
let interpolatedRaw = #"Value: \(42)"#
let multilineRaw = """
    Line 1
    Line 2
    """

// Type inference
let inferredString = "Hello" // String
let inferredInt = 42 // Int
let inferredArray = [1, 2, 3] // [Int]

// Type aliases
typealias AudioSample = UInt16
typealias Dictionary = [String: Any]

print("=== Basic Types ===")
print("Int: \(int), Float: \(float), String: \(string)")
print("Hex: \(hexInt), Binary: \(binaryInt)")
print("Raw string: \(rawString)")
print("Inferred types: \(inferredString), \(inferredInt), \(inferredArray)")
