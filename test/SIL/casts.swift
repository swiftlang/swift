// RUN: %swift -emit-sil %s | FileCheck %s
// 'is' unimplemented in SILGen -Joe
// XFAIL: *

class B { }
class D : B { }

// CHECK: sil @upcast
func upcast(d:D) -> B {
  return d as B
}
// CHECK: sil @downcast
func downcast(b:B) -> D {
  return b as! D
}
// CHECK: sil @isa
func isa(b:B) -> Bool {
  return b is D
}
// CHECK: sil @downcast_archetype
func downcast_archetype<T:B>(b:B) -> T {
  return b as! T
}
// CHECK: sil @is_archetype
func is_archetype<T:B>(b:B) -> Bool {
  return b is T
}
