// RUN: %target-swift-frontend -Xcc -fno-objc-msgsend-selector-stubs -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop
import Foundation

public class Demo: NSObject {
  public override init() {
          super.init()

          print(Demo.locale)
  }
  // This used to crash a -O.
  private static let locale = staticInlineFun()

}

@inline(never)
func testDemo() {
  let a = Demo()
}

testDemo()

// Make sure the clang importer puts the selectors and co into the llvm.compiler used variable.
//
// This needs Clang to emit the message sends in StaticInline.h as a selector
// load plus a call to objc_msgSend, so selector-reference globals exist in the
// first place. Where -fobjc-msgsend-selector-stubs is the default (AArch64
// targets linked with ld64-811.2 or newer) the sends go through per-selector
// linker stubs instead and no such globals are emitted, hence the explicit
// -fno-objc-msgsend-selector-stubs above. The mechanism under test -- merging
// Clang's llvm.compiler.used into Swift's -- is independent of the ObjC
// dispatch strategy.

// CHECK: @llvm.compiler.used = appending global [{{.*}} x ptr] [{{.*}} @"OBJC_CLASSLIST_REFERENCES_$_"{{.*}}@OBJC_METH_VAR_NAME_{{.*}}@OBJC_SELECTOR_REFERENCES_{{.*}}@OBJC_METH_VAR_NAME_.{{.*}}@OBJC_SELECTOR_REFERENCES_.{{.*}}]

