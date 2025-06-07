// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -I %S/Inputs) | %FileCheck %s

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

func printStruct() {
    let s = S()
    print(s.getPrivVec())
    print(s.getProtStr())
    print(s.pubStr)
    print(s.pubVec)
    print(s)
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

printStruct()
// CHECK: vector<{{.*}}.basic_string<CChar, {{.*}}, {{.*}}>, {{.*}}>()
// CHECK: basic_string<CChar, {{.*}}, {{.*}}>()
// CHECK: basic_string<CChar, {{.*}}, {{.*}}>()
// CHECK: vector<{{.*}}.basic_string<CChar, {{.*}}, {{.*}}>, {{.*}}>()
// CHECK: S(pubStr: {{.*}}.basic_string<CChar, {{.*}}, {{.*}}>(), pubVec: {{.*}}.vector<{{.*}}.basic_string<CChar, {{.*}}, {{.*}}>, {{.*}}>())
