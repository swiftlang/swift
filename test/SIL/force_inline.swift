// RUN: %swift -emit-sil %s | FileCheck %s

func [force_inline] easy(x:String) -> String {
  return x
}

func [force_inline] nested(x:String) -> String {
  return easy(x)
}

class C { func foo(x:Int) -> Int { return x } }

func [force_inline] has_cleanups(x:Int) -> Int {
  var c = C()
  return c.foo(x)
}

protocol Ansible {}

func [force_inline] address_only_return(x:Ansible) -> Ansible {
  return x
}

func [force_inline] void_return(b:Bool) {
  if b {
    return
  }
}

// CHECK: sil @_T12force_inline3fooFT1aSS1bSi1cPS_7Ansible_TSSSi_
func foo(a:String, b:Int, c:Ansible) -> (String, Int) {
  // CHECK-NOT: function_ref {{.*}} @_T12force_inline4easyFT1xSS_SS
  var a2 = easy(a)

  // CHECK-NOT: function_ref {{.*}} @_T12force_inline6nestedFT1xSS_SS
  nested(a)
  // CHECK: br

  // Can use force-inline functions as values and they get called normally.
  var f = easy
  f(a)

  // CHECK-NOT: function_ref {{.*}} @_T12force_inline12has_cleanupsFT1xSi_Si
  var b2 = has_cleanups(b)

  // CHECK-NOT: function_ref {{.*}} @_T12force_inline19address_only_returnFT1xPS_7Ansible_S0_
  var c2 = address_only_return(c)

  // CHECK-NOT: function_ref {{.*}} @_T12force_inline19address_only_returnFT1xPS_7Ansible_S0_
  void_return(true)

  return (a2, b2)
}
