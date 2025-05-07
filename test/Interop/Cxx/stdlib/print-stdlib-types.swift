// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out -Xfrontend -enable-experimental-cxx-interop -I %S/Inputs
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --check-prefix=CHECK

// REQUIRES: executable_test
// XFAIL: OS=windows-msvc
// FIXME: We can't import std::unique_ptr or std::shared_ptr properly on Windows (https://github.com/apple/swift/issues/70226)

import StdStringAndVector
import StdUniquePtr

func printStdString(s: std.string) {
    print(s)
    let swiftString = String(s)
    print(swiftString)
}

func printStdUniquePtrOfInt() {
    let uint = makeInt()
    print(uint.pointee)
}

func printStdUniquePtrOfString() {
    let ustring = makeString()
    print(ustring.pointee)
}

func printStdSharedPtrOfInt() {
    let sint = makeIntShared()
    print(sint.pointee)
    print(sint)
}

func printStdSharedPtrOfString() {
    let sstring = makeStringShared()
    print(sstring.pointee)
    print(sstring)
}

func printStdVectorOfInt() {
    let vecOfInt = makeVecOfInt()
    print(vecOfInt[0])
    print(vecOfInt)
}

func printStdVectorOfString() {
    let vecOfString = makeVecOfString()
    print(vecOfString[0])
    print(vecOfString)
}

printStdString(s: "Hello")
// CHECK: basic_string<CChar, {{.*}}, {{.*}}>()
// CHECK: Hello

printStdUniquePtrOfInt()
// CHECK: 42
printStdUniquePtrOfString()
// CHECK: basic_string<CChar, {{.*}}, {{.*}}>()

printStdSharedPtrOfInt()
// CHECK: 42
// CHECK: shared_ptr<CInt>()
printStdSharedPtrOfString()
// CHECK: basic_string<CChar, {{.*}}, {{.*}}>()
// CHECK: shared_ptr<{{.*}}.basic_string<CChar, {{.*}}, {{.*}}>>

printStdVectorOfInt()
// CHECK: 1
// CHECK: vector<CInt, {{.*}}>()
printStdVectorOfString()
// CHECK: basic_string<CChar, {{.*}}, {{.*}}>()
// CHECK: vector<{{.*}}.basic_string<CChar, {{.*}}, {{.*}}>, {{.*}}>()
