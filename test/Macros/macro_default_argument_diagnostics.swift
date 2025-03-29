// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -enable-bare-slash-regex

public typealias Stringified<T> = (T, String)

@freestanding(expression)
macro stringify<T>(_ value: T) -> Stringified<T> = #externalMacro(
    module: "MacroDefinition", type: "StringifyMacro"
)

struct MyType { }
typealias MyInt = Int
struct MyLiteral: ExpressibleByStringLiteral, _ExpressibleByColorLiteral, _ExpressibleByImageLiteral, _ExpressibleByFileReferenceLiteral {
    init(stringLiteral value: StringLiteralType) {}
    init(_colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) {}
    init(imageLiteralResourceName: String) {}
    init(fileReferenceLiteralResourceName: String) {}
}

// MARK: literals

func testNil(nil: Stringified<MyLiteral?> = #stringify(nil)) {}
func testBool(true: Stringified<Bool> = #stringify(true),
              false: Stringified<Bool> = #stringify(false)) {}
func testInt(positive: Stringified<UInt64> = #stringify(1_000_001),
             zero: Stringified<MyInt> = #stringify(0x0),
             negative: Stringified<Int32> = #stringify(-0o21)) {}
func testFloat(double: Stringified<Double> = #stringify(-0xC.3p0),
               float: Stringified<Float> = #stringify(00003.14159)) {}
func testString(literal: Stringified<MyLiteral> = #stringify("🐨")) {}
func testMagic(fileID: Stringified<String> = #stringify(#fileID),
               filePath: Stringified<String> = #stringify(#filePath),
               file: Stringified<String> = #stringify(#file),
               function: Stringified<String> = #stringify(#function),
               line: Stringified<Int> = #stringify(#line),
               column: Stringified<Int> = #stringify(#column),
               dso: Stringified<UnsafeRawPointer> = #stringify(#dsohandle)) {}
@available(SwiftStdlib 5.7, *)
func testRegex(literal: Stringified<Regex<Substring>> = #stringify(/foo/)) {}
func testObject(color: Stringified<MyLiteral> = #stringify(#colorLiteral(red: 0.4, green: 0.8, blue: 1, alpha: 1)),
                image: Stringified<MyLiteral> = #stringify(#imageLiteral(resourceName: "swift.png")),
                file: Stringified<MyLiteral> = #stringify(#fileLiteral(resourceName: "main.swift"))) {}

// MARK: not literal

let myString = "oops"

// expected-error@+1{{only literals are permitted}}
func testIdentifier(notOkay: Stringified<String> = #stringify(myString)) {}

// expected-error@+1{{only literals are permitted}}
func testString(interpolated: Stringified<String> = #stringify("Hello \(0b10001)")) {}

// expected-error@+1{{default argument value of type '(Int, String)' cannot be converted to type 'Int'}}
func testReturn(wrongType: Int = #stringify(0)) {}
