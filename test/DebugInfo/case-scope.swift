// RUN: %target-swift-frontend -module-name a -parse-as-library -emit-sil -g %s | %FileCheck %s

public enum E<T> {
case A(T)
case B(T)
case C(String)
case D(T, T, T)
}

func sink<T>(_ t: T) {}

public func f<T>(_ e: E<T>) -> [T] {
  switch e {
  case .A(let a), .B(let a): return [a]
  case .D(let a, _, let c):  return [a, c]
  default:                   return []
  }
}

// CHECK: sil_scope [[F:[0-9]+]] { loc "{{.*}}":12:13 parent @$s1a1fySayxGAA1EOyxGlF
// CHECK: sil_scope [[S0:[0-9]+]] { loc "{{.*}}":13:3 parent [[F]] }
// CHECK: sil_scope [[A0:[0-9]+]] { loc "{{.*}}":14:3 parent [[S0]] }
// CHECK: sil_scope [[A1:[0-9]+]] { loc "{{.*}}":15:3 parent [[S0]] }
// CHECK: alloc_stack {{.*}} $T, let, name "a", {{.*}}:14:15, scope [[A0]]
// CHECK: alloc_stack {{.*}} $T, let, name "a", {{.*}}:14:26, scope [[A0]]
// CHECK: alloc_stack {{.*}} $T, let, name "a", {{.*}}:15:15, scope [[A1]]
