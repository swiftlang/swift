// RUN: %target-swift-ide-test -print-module -module-to-print=NonPublicShadow -print-access -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers | %FileCheck %s
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

// We only check the module interface of Shadow to keep this test concise

// CHECK:      public struct Shadow {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public func publPublShadowed() -> Return
// CHECK-NEXT:   public func protPublShadowed() -> Return
// CHECK-NEXT:   public func privPublShadowed() -> Return
// CHECK-NEXT:   private func publPrivShadowed() -> Return
// CHECK-NEXT:   private func protPrivShadowed() -> Return
// CHECK-NEXT:   private func privPrivShadowed() -> Return

// Currently, ImportDecl.cpp::loadAllMembersOfRecordDecl() does not correctly
// handle multiple inheritance, so it only loads one of each ambiguous member.

// TODO:         public func publOrPriv() -> Return
// TODO:         @available(*, unavailable, message: "this base member is not accessible because it is private")
// TODO:         private func publOrPriv() -> Return

// TODO:         private func protOrPriv() -> Return
// TODO:         @available(*, unavailable, message: "this base member is not accessible because it is private")
// TODO:         private func protOrPriv() -> Return

// TODO:         public func publOrProt() -> Return
// TODO:         private func publOrProt() -> Return
// CHECK:      }
