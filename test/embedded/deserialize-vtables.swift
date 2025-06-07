// RUN: %target-run-simple-swift(-enable-experimental-feature Extern -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Extern -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize -enable-experimental-feature Extern -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

@main
struct Main {
    static func main() {
        StaticString("Hello, World!").asUTF8Array.print()
        // CHECK: Hello, World!
    }
}

extension StaticString {
    var asUTF8Array: [UInt8] {
        Array(UnsafeBufferPointer(start: utf8Start, count: utf8CodeUnitCount))
    }
}

@_extern(c, "putchar")
@discardableResult
func putchar(_: CInt) -> CInt

extension Array<UInt8> {
    func print() {
        for byte in self {
            putchar(CInt(byte))
        }
        putchar(10)
    }
}
