// RUN: %target-swift-frontend %use_no_opaque_pointers -disable-type-layout -enable-library-evolution -disable-availability-checking -emit-ir -primary-file %s | %FileCheck %s
// RUN: %target-swift-frontend -disable-type-layout -enable-library-evolution -disable-availability-checking -emit-ir -primary-file %s

public protocol E {}

public struct Pair<T, V> : E {
  var fst : T
  var snd : V

  public init(_ f: T, _ s: V) {
    self.fst = f
    self.snd = s
  }

  public func foobar() -> some E {
    return self
  }
}

@inlinable
public func usePair<T, V>(_ t: T, _ v: V) {
  var x = Pair(t, v)
  let q = x.foobar()
  let u = x.foobar()
  let p = Pair(q, u)
  print(p)
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s31opaque_result_type_substitution7usePairyyx_q_tr0_lF"({{.*}}, %swift.type* %T, %swift.type* %V)
// CHECK:  [[PAIR_TV:%.*]] = call swiftcc %swift.metadata_response @"$s31opaque_result_type_substitution4PairVMa"({{.*}}, %swift.type* %T, %swift.type* %V)
// CHECK:  [[MD:%.*]] = extractvalue %swift.metadata_response [[PAIR_TV]], 0
// CHECK:  [[PAIR_OPAQUE:%.*]] = call swiftcc %swift.metadata_response @"$s31opaque_result_type_substitution4PairVMa"({{.*}}, %swift.type* [[MD]], %swift.type* [[MD]])
// CHECK:  [[MD2:%.*]] = extractvalue %swift.metadata_response [[PAIR_OPAQUE]], 0
// CHECK:  call {{.*}}* @"$s31opaque_result_type_substitution4PairVyACyxq_GADGr0_lWOh"({{.*}}, %swift.type* {{.*}}, %swift.type* [[MD2]])
