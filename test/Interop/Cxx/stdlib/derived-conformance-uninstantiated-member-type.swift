// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test \
// RUN:   -print-module \
// RUN:   -module-to-print=DerivedConformanceUninstantiatedMemberType \
// RUN:   -source-filename=x \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -I %S/Inputs | %FileCheck %s

// REQUIRES: swift_feature_ImportCxxMembersLazily

// CHECK: struct vector<CInt> : CxxVector {
// CHECK:   typealias const_iterator = __gnu_cxx.__normal_iterator<UnsafePointer<CInt>, std.vector<CInt>>
// CHECK:   typealias RawIterator = __gnu_cxx.__normal_iterator<UnsafePointer<CInt>, std.vector<CInt>>
// CHECK: }
