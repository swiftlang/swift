// RUN: %target-run-simple-swift(-enable-experimental-feature CDecl) > %t.out
// RUN: %FileCheck --input-file %t.out %s

// REQUIRES: swift_feature_CDecl
// REQUIRES: executable_test

@cdecl
enum CDecl8: UInt8 {
case a
case b
}

@cdecl
enum CDecl16: UInt16 {
case a
case b
}

@cdecl(SomeName)
enum CDecl32: UInt32 {
case a
case b
}

@objc
enum ObjCEnum: UInt32 {
case a
case b
}

enum SwiftEnum: Int32 {
case a
case b
}

print("@cdecl enum 8 is \(MemoryLayout<CDecl8>.size) bytes")
// CHECK: @cdecl enum 8 is 1 bytes
print("@cdecl enum 16 is \(MemoryLayout<CDecl16>.size) bytes")
// CHECK: @cdecl enum 16 is 2 bytes
print("@cdecl enum 32 is \(MemoryLayout<CDecl32>.size) bytes")
// CHECK: @cdecl enum 32 is 4 bytes
print("@objc enum is \(MemoryLayout<ObjCEnum>.size) bytes")
// CHECK: @objc enum is 4 bytes
print("Swift enum is \(MemoryLayout<SwiftEnum>.size) bytes")
// CHECK: Swift enum is 1 bytes
