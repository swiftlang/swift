// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck -check-prefix=INTEL64 %s
// REQUIRES: ARM
// RUN: %swift -triple armv7-apple-ios7 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck -check-prefix=ARM32 %s
import gizmo

class Foo {
  // INTEL64: define void @_TC8abitypes3Foo3barfS0_FT_VSC4Rect(%VSC4Rect* noalias sret, %C8abitypes3Foo*) {
  // FIXME: This is the wrong direct return type for x86_64 and will be fixed by
  //        a future commit.
  // INTEL64: define internal { float, float, float, float } @_TToC8abitypes3Foo3barfS0_FT_VSC4Rect(%C8abitypes3Foo*, i8*) unnamed_addr {
  // ARM32:   define void @_TC8abitypes3Foo3barfS0_FT_VSC4Rect(%VSC4Rect* noalias sret, %C8abitypes3Foo*) {
  // ARM32:   define internal void @_TToC8abitypes3Foo3barfS0_FT_VSC4Rect(%VSC4Rect* noalias sret, %C8abitypes3Foo*, i8*) unnamed_addr {
  @objc func bar() -> Rect {
    return Rect(1,2,3,4)
  }

  // INTEL64:      define float @_TC8abitypes3Foo4barcfS0_FT1pCSo15NSStructReturns_Sf(%CSo15NSStructReturns*, %C8abitypes3Foo*) {
  // INTEL64:      load i8** @"\01L_selector(newRect)", align 8
  // FIXME: This is the wrong direct return type for x86_64 and will be fixed by
  //        a future commit.
  // INTEL64-NEXT: call { float, float, float, float } bitcast (void ()* @objc_msgSend
  // ARM32:        define float @_TC8abitypes3Foo4barcfS0_FT1pCSo15NSStructReturns_Sf(%CSo15NSStructReturns*, %C8abitypes3Foo*) {
  // ARM32:        load i8** @"\01L_selector(newRect)", align 4
  // ARM32-NEXT:   call void bitcast (void ()* @objc_msgSend_stret to void (%VSC4Rect*, %CSo15NSStructReturns*, i8*)*)
  func barc(p : NSStructReturns) -> Float {
    return p.newRect().y
  }

  // INTEL64: define { double, double, double } @_TC8abitypes3Foo3bazfS0_FT_VSC4Trio(%C8abitypes3Foo*) {
  // INTEL64: define internal void @_TToC8abitypes3Foo3bazfS0_FT_VSC4Trio(%VSC4Trio* noalias sret, %C8abitypes3Foo*, i8*) unnamed_addr {
  // ARM32:   define { double, double, double } @_TC8abitypes3Foo3bazfS0_FT_VSC4Trio(%C8abitypes3Foo*) {
  // ARM32:   define internal void @_TToC8abitypes3Foo3bazfS0_FT_VSC4Trio(%VSC4Trio* noalias sret, %C8abitypes3Foo*, i8*) unnamed_addr {
  @objc func baz() -> Trio {
    return Trio(1.0,2.0,3.0)
  }

  // INTEL64:      define double @_TC8abitypes3Foo4bazcfS0_FT1pCSo15NSStructReturns_Sd(%CSo15NSStructReturns*, %C8abitypes3Foo*) {
  // INTEL64:      load i8** @"\01L_selector(newTrio)", align 8
  // INTEL64-NEXT: call void bitcast (void ()* @objc_msgSend_stret to void (%VSC4Trio*, %CSo15NSStructReturns*, i8*)*)
  // ARM32:        define double @_TC8abitypes3Foo4bazcfS0_FT1pCSo15NSStructReturns_Sd(%CSo15NSStructReturns*, %C8abitypes3Foo*) {
  // ARM32:        load i8** @"\01L_selector(newTrio)", align 4
  // ARM32-NEXT:   call void bitcast (void ()* @objc_msgSend_stret to void (%VSC4Trio*, %CSo15NSStructReturns*, i8*)*)
  func bazc(p : NSStructReturns) -> Double {
    return p.newTrio().j
  }
}
