// RUN: %swift -emit-silgen -emit-verbose-sil %s | FileCheck %s

// Test if 'transparent' atribute gets propagated correctly to apply instructions.

// Test that the attribute gets set on default argument generators.
func [transparent] transparentFuncWithDefaultArgument (x :Int = 1) -> Int {
  return x
}
func useTransparentFuncWithDefaultArgument() ->Int {
  return transparentFuncWithDefaultArgument();

  // CHECK-LABEL: sil @_T21transparent_attribute37useTransparentFuncWithDefaultArgumentFT_Si
  // CHECK: apply [transparent] {{.*}} line:10:44
  // CHECK: apply [transparent] {{.*}} line:10:44
  // CHECK: return
  
}

func transparentFuncWithoutDefaultArgument (x :Int = 1) -> Int {
  return x
}
func useTransparentFuncWithoutDefaultArgument() ->Int {
  return transparentFuncWithoutDefaultArgument();

  // CHECK-LABEL: sil @_T21transparent_attribute40useTransparentFuncWithoutDefaultArgumentFT_Si
  // CHECK: apply {{.*}} line:23:47
  // CHECK-NOT: transparent
  // CHECK: apply {{.*}} line:23:47
  // CHECK: return
  
}
