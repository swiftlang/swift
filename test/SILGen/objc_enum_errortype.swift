// RUN: %target-swift-frontend -emit-silgen %s -import-objc-header %S/Inputs/objc_enum_errortype.h | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: sil @_TFE19objc_enum_errortypeVSC7AXErrorg5_codeSi

extension AXError: Error { }
