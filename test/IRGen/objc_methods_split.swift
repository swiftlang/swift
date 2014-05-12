// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -split-objc-selectors -target x86_64-apple-darwin10 %s -emit-ir | FileCheck %s

// CHECK: @"\01L_selector_data(bazWithInt:)" = internal constant [{{[0-9]+}} x i8] c"bazWithInt:\00", section "__TEXT,__objc_methname,cstring_literals", align 1
// CHECK: @"\01L_selector_data(fooWithInt:)" = internal constant [12 x i8] c"fooWithInt:\00", section "__TEXT,__objc_methname,cstring_literals", align 1

class Foo {
  @objc func baz(`int: Int) {}

  @objc(fooWithInt:) func foo(`int: Int) {}
}


