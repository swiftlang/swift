// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

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

@_silgen_name("putchar")
func putchar(_: UInt8)

extension Array<UInt8> {
    func print() {
        for byte in self {
            putchar(byte)
        }
        putchar(10)
    }
}
