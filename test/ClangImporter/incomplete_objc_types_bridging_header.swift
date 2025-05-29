// RUN: %target-swift-frontend -import-objc-header %S/Inputs/incomplete_objc_types_bridging_header.h -enable-upcoming-feature ImportObjcForwardDeclarations -enable-objc-interop -typecheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

let foo = CFunctionReturningAForwardDeclaredInterface()
CFunctionTakingAForwardDeclaredInterface(foo)
