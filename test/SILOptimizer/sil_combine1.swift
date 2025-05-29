// RUN: %target-swift-frontend %s -O -Xllvm -sil-print-types -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend %s -O -Xllvm -sil-print-types -enable-ossa-modules -emit-sil | %FileCheck %s

func curry<T1, T2, T3, T4>(_ f: @escaping (T1, T2, T3) -> T4) -> (T1) -> (T2) -> (T3) -> T4 {
  return { x in { y in { z in f(x, y, z) } } }
}

public protocol P {
  func val() -> Int32
}

struct CP: P {
  let v: Int32

  func val() -> Int32 {
     return v
  }

  init(_ v: Int32) {
    self.v = v
  }
}

func compose(_ x: P, _ y: P, _ z: P) -> Int32 {
  return x.val() + y.val() + z.val()
}

//CHECK-LABEL: sil [noinline] @$s12sil_combine120test_compose_closures5Int32VyF : $@convention(thin) () -> Int32 {
//CHECK: [[OEADDR:%.*]] = open_existential_addr immutable_access {{%.*}} : $*any P to $*@opened
//CHECK: [[ADDRCAST:%.*]] = unchecked_addr_cast [[OEADDR]] : $*@opened
//CHECK: struct_element_addr [[ADDRCAST]] : $*CP, #CP.v
@inline(never)
public func test_compose_closure() -> Int32 {
  let insult = curry(compose)(CP(1))(CP(2))
  let gs = insult(CP(3))
  return gs
}

// CHECK-LABEL: sil @$s12sil_combine122remove_partial_applies_3key5valueySDySSSiGSgz_SSSitF :
// CHECK-NOT:     partial_apply
// CHECK:       } // end sil function '$s12sil_combine122remove_partial_applies_3key5valueySDySSSiGSgz_SSSitF'
public func remove_partial_applies(_ dict: inout [String: Int]?, key: String, value: Int) {
  dict?[key, default: value] = value 
}
