// RUN: %target-swift-ide-test -print-module -module-to-print=UninstantiatableSpecialMembers -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: struct HasUninstantiatableCopyConstructor<CInt> {
// CHECK: }
// CHECK: typealias NonCopyableInst = HasUninstantiatableCopyConstructor<CInt>

// CHECK: struct DerivedUninstantiatableCopyConstructor<CInt> {
// CHECK: }
// CHECK: typealias DerivedNonCopyableInst = DerivedUninstantiatableCopyConstructor<CInt>
