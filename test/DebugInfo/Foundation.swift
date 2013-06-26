// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -triple x86_64-apple-darwin10 -emit-llvm -g %s | FileCheck %s
// REQUIRES: sdk
// CHECK: DW_TAG_compile_unit
import ObjectiveC
import Foundation

func [asmname="swift_StringToNSString"]
convertStringToNSString(string : [byref] String) -> NSString

class MyObject : NSObject {
  func foo(obj: MyObject) {
    return obj.foo(obj);
  }
}
