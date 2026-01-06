// RUN: %target-swift-ide-test -print-module -module-to-print=FakeToolchain -tools-directory %S/Inputs/fake-toolchain/bin -source-filename=x -enable-experimental-cxx-interop -Xcc -stdlib=libc++ | %FileCheck %s

// Clang driver on Windows doesn't support -stdlib=libc++
// XFAIL: OS=windows-msvc
// Android NDK layout might need more flags to find libc++
// XFAIL: OS=linux-androideabi
// XFAIL: OS=linux-android
// XFAIL: OS=freebsd

// CHECK: enum FakeNamespace {
// CHECK:   static func foo(_ x: Int32)
// CHECK: }
// CHECK: enum std {
// CHECK:   typealias size_t = Int
// CHECK: }

