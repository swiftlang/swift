
// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

infix operator +=+ : AdditionPrecedence
extension RangeReplaceableCollection {
   public static func +=+ <
     Other : Sequence
   >(lhs: Self, rhs: Other) -> Self
    where Element == Other.Element {
     fatalError()
   }

   public static func +=+ <
     Other : Sequence
   >(lhs: Other, rhs: Self) -> Self
    where Element == Other.Element {
     fatalError()
   }

   public static func +=+ <
    Other : RangeReplaceableCollection
   >(lhs: Self, rhs: Other) -> Self
    where Element == Other.Element {
     fatalError()
   }
}

func rdar36333688(_ first: Int, _ rest: Int...) {
  // CHECK: function_ref @{{.*}} : $@convention(method) <τ_0_0 where τ_0_0 : RangeReplaceableCollection><τ_1_0 where τ_1_0 : RangeReplaceableCollection, τ_0_0.Element == τ_1_0.Element> (@in_guaranteed τ_0_0, @in_guaranteed τ_1_0, @thick τ_0_0.Type) -> @out τ_0_0
  let _ = [first] +=+ rest
}
