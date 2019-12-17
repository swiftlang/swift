// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

infix operator +++

protocol Twig {
  static func +++(lhs: Self, rhs: Self)
}

struct Branch : Twig {
  @_implements(Twig, +++(_:_:))
  static func doIt(_: Branch, _: Branch) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s18protocol_operators9useBranchyyAA0D0VF : $@convention(thin) (Branch) -> () {
// CHECK: function_ref @$s18protocol_operators6BranchV4doItyyAC_ACtFZ : $@convention(method) (Branch, Branch, @thin Branch.Type) -> ()
// CHECK: return
func useBranch(_ b: Branch) {
  b +++ b
}

class Stick : Twig {
  static func +++(lhs: Stick, rhs: Stick) {}
}

class Stuck : Stick, ExpressibleByIntegerLiteral {
  typealias IntegerLiteralType = Int

  required init(integerLiteral: Int) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s18protocol_operators8useStickyyAA5StuckC_AA0D0CtF : $@convention(thin) (@guaranteed Stuck, @guaranteed Stick) -> () {
// CHECK: function_ref @$s18protocol_operators5StickC3pppoiyyAC_ACtFZ : $@convention(method) (@guaranteed Stick, @guaranteed Stick, @thick Stick.Type) -> ()
// CHECK: function_ref @$s18protocol_operators5StickC3pppoiyyAC_ACtFZ : $@convention(method) (@guaranteed Stick, @guaranteed Stick, @thick Stick.Type) -> ()
// CHECK: witness_method $Stuck, #Twig."+++"!1 : <Self where Self : Twig> (Self.Type) -> (Self, Self) -> () : $@convention(witness_method: Twig) <τ_0_0 where τ_0_0 : Twig> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK: return
func useStick(_ a: Stuck, _ b: Stick) {
  _ = a +++ b
  _ = b +++ b
  _ = a +++ 5
}

class Twine<X> : Twig {
  static func +++(lhs: Twine, rhs: Twine) {}
}

class Rope : Twine<Int>, ExpressibleByIntegerLiteral {
  typealias IntegerLiteralType = Int

  required init(integerLiteral: Int) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s18protocol_operators7useRopeyyAA0D0C_ADtF : $@convention(thin) (@guaranteed Rope, @guaranteed Rope) -> () {
// CHECK: function_ref @$s18protocol_operators5TwineC3pppoiyyACyxG_AEtFZ : $@convention(method) <τ_0_0> (@guaranteed Twine<τ_0_0>, @guaranteed Twine<τ_0_0>, @thick Twine<τ_0_0>.Type) -> ()
// CHECK: function_ref @$s18protocol_operators5TwineC3pppoiyyACyxG_AEtFZ : $@convention(method) <τ_0_0> (@guaranteed Twine<τ_0_0>, @guaranteed Twine<τ_0_0>, @thick Twine<τ_0_0>.Type) -> ()
// CHECK: witness_method $Rope, #Twig."+++"!1 : <Self where Self : Twig> (Self.Type) -> (Self, Self) -> () : $@convention(witness_method: Twig) <τ_0_0 where τ_0_0 : Twig> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
func useRope(_ r: Rope, _ s: Rope) {
  _ = r +++ s
  _ = s +++ s
  _ = r +++ 5
}
