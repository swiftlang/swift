// RUN: %target-swift-frontend -enable-objc-interop -disable-objc-attr-requires-foundation-module -emit-ir -parse-as-library -parse-stdlib %s -module-name M -o - %s | FileCheck %s
// REQUIRES: objc_interop

@objc
public class C {
  public func f() {}
}

// CHECK: define internal void @_TToFC1M1C1ffT_T_

