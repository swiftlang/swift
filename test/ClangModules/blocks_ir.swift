// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -constraint-checker %s -emit-llvm -o - | FileCheck %s
// RUN: ls -lR %t/clang-module-cache | grep blocks.pcm

import blocks

func getLastLine(someNSString : NSString) -> NSString {
  // CHECK: define %CSo8NSString* @_T9blocks_ir11getLastLineFT12someNSStringCSo8NSString_S0_(%CSo8NSString* %someNSString)
  var result : NSString

  someNSString.enumerateLinesUsingBlock(func(s) {
    result = s
  })
  // CHECK: [[SELECTOR:%.*]] = load i8** @"\01L_selector(enumerateLinesUsingBlock:)", align 8
  // CHECK: [[BLOCK:%.*]] = call %objc_object* @_TTbbCSo8NSStringT_(i8* bitcast (void (%CSo8NSString*, %swift.refcounted*)* [[CLOSURE:@.*]] to i8*), %swift.refcounted* [[DATA:%.*]])
  // CHECK: call void bitcast (void ()* @objc_msgSend to void (%CSo8NSString*, i8*, %objc_object*)*)(%CSo8NSString* [[INPUT:%.*]], i8* [[SELECTOR]], %objc_object* [[BLOCK]])
  // CHECK: call void @objc_release(%objc_object* [[BLOCK]]) nounwind
  // CHECK-NOT: call void @swift_release(%swift.refcounted* [[DATA]])

  return result
}
