// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test \
// RUN:   -print-module \
// RUN:   -module-to-print=DerivedConformanceUninstantiatedMemberType \
// RUN:   -source-filename=x \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -I %S/Inputs | %FileCheck %s

// FIXME: The synthesized conformance of std::vector to CxxVector on android-armv7 is invalid
// https://github.com/swiftlang/swift/issues/93524
// XFAIL: OS=linux-androideabi

// CHECK: struct vector<CInt> : CxxVector {
// CHECK:   typealias const_iterator = __gnu_cxx.__normal_iterator<UnsafePointer<CInt>, std.vector<CInt>>
// CHECK:   typealias RawIterator = __gnu_cxx.__normal_iterator<UnsafePointer<CInt>, std.vector<CInt>>
// CHECK: }
