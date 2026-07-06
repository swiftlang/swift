
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name nested_generics -Xllvm -sil-full-demangle  -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -module-name nested_generics -Xllvm -sil-full-demangle -parse-as-library %s > /dev/null
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -module-name nested_generics -Xllvm -sil-full-demangle -O -parse-as-library %s > /dev/null
// RUN: %target-swift-emit-ir -module-name nested_generics -Xllvm -sil-full-demangle -parse-as-library %s > /dev/null

// TODO:
// - test generated SIL -- mostly we're just testing mangling here
// - class_method calls
// - witness_method calls
// - inner generic parameters on protocol requirements
// - generic parameter list on method in nested type
// - types nested inside unconstrained extensions of generic types

protocol Pizza : class {
  associatedtype Topping
}

protocol HotDog {
  associatedtype Condiment
}

protocol CuredMeat {}

// Generic nested inside generic

struct Lunch<T : Pizza> where T.Topping : CuredMeat {
  struct Dinner<U : HotDog> where U.Condiment == Deli<Pepper>.Mustard {
    let firstCourse: T
    let secondCourse: U?

    var leftovers: T
    var transformation: (T) -> U

    func coolCombination(t: T.Topping, u: U.Condiment) {
      func nestedGeneric<X, Y>(x: X, y: Y) -> (X, Y) {
        return (x, y)
      }
      _ = nestedGeneric(x: t, y: u)
    }
  }
}

// CHECK-LABEL: // nested_generics.Lunch.Dinner.coolCombination(t: A.Topping, u: nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s15nested_generics5LunchV6DinnerV15coolCombination1t1uy7ToppingQz_AA4DeliC7MustardOyAA6PepperV_GtF : $@convention(method) <T where T : Pizza, T.Topping : CuredMeat><U where U : HotDog, U.Condiment == Deli<Pepper>.Mustard> (@in_guaranteed T.Topping, Deli<Pepper>.Mustard, @in_guaranteed Lunch<T>.Dinner<U>) -> ()

// CHECK-LABEL: // nestedGeneric #1 <A><A1><A2, B2 where A: nested_generics.Pizza, A1: nested_generics.HotDog, A.Topping: nested_generics.CuredMeat, A1.Condiment == nested_generics.Deli<nested_generics.Pepper>.Mustard>(x: A2, y: B2) -> (A2, B2) in nested_generics.Lunch.Dinner.coolCombination(t: A.Topping, u: nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()
// CHECK-LABEL: sil private [ossa] @$s15nested_generics5LunchV6DinnerV15coolCombination1t1uy7ToppingQz_AA4DeliC7MustardOyAA6PepperV_GtF0A7GenericL_1x1yqd0___qd0_0_tqd0___qd0_0_tAA5PizzaRzAA6HotDogRd__AA9CuredMeatAJRQAQ9CondimentRtd__r__0_lF : $@convention(thin) <T where T : Pizza, T.Topping : CuredMeat><U where U : HotDog, U.Condiment == Deli<Pepper>.Mustard><X, Y> (@in_guaranteed X, @in_guaranteed Y) -> (@out X, @out Y)

// CHECK-LABEL: // nested_generics.Lunch.Dinner.init(firstCourse: A, secondCourse: Swift.Optional<A1>, leftovers: A, transformation: (A) -> A1) -> nested_generics.Lunch<A>.Dinner<A1>
// CHECK-LABEL: sil hidden [ossa] @$s15nested_generics5LunchV6DinnerV11firstCourse06secondF09leftovers14transformationAEyx_qd__Gx_qd__Sgxqd__xctcfC : $@convention(method) <T where T : Pizza, T.Topping : CuredMeat><U where U : HotDog, U.Condiment == Deli<Pepper>.Mustard> (@owned T, @in Optional<U>, @owned T, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject> (@guaranteed τ_0_0) -> @out τ_0_1 for <T, U>, @thin Lunch<T>.Dinner<U>.Type) -> @out Lunch<T>.Dinner<U>

// Non-generic nested inside generic

class Deli<Spices> : CuredMeat {

  class Pepperoni : CuredMeat {}
  struct Sausage : CuredMeat {}

  enum Mustard {
    case Yellow
    case Dijon
    case DeliStyle(Spices)
  }
}

// CHECK-LABEL: // nested_generics.Deli.Pepperoni.init() -> nested_generics.Deli<A>.Pepperoni
// CHECK-LABEL: sil hidden [ossa] @$s15nested_generics4DeliC9PepperoniCAEyx_Gycfc : $@convention(method) <Spices> (@owned Deli<Spices>.Pepperoni) -> @owned Deli<Spices>.Pepperoni

// Typealiases referencing outer generic parameters

struct Pizzas<Spices> {
  class NewYork : Pizza {
    typealias Topping = Deli<Spices>.Pepperoni
  }

  class DeepDish : Pizza {
    typealias Topping = Deli<Spices>.Sausage
  }
}

class HotDogs {
  struct Bratwurst : HotDog {
    typealias Condiment = Deli<Pepper>.Mustard
  }
  struct American : HotDog {
    typealias Condiment = Deli<Pepper>.Mustard
  }
}

// Local type in extension of type in another module
extension String {
  func foo() {
    // CHECK-LABEL: // init(material: A) -> Cheese #1 in (extension in nested_generics):Swift.String.foo() -> ()<A> in Cheese #1 in (extension in nested_generics):Swift.String.foo() -> ()
    // CHECK-LABEL: sil private [ossa] @$sSS15nested_genericsE3fooyyF6CheeseL_V8materialADyxGx_tcfC
    struct Cheese<Milk> {
      let material: Milk
    }
    let _ = Cheese(material: "cow")
  }
}

// Local type in extension of type in same module
extension HotDogs {
  func applyRelish() {
    // CHECK-LABEL: // init(material: A) -> Relish #1 in nested_generics.HotDogs.applyRelish() -> ()<A> in Relish #1 in nested_generics.HotDogs.applyRelish() -> ()
    // CHECK-LABEL: sil private [ossa] @$s15nested_generics7HotDogsC11applyRelishyyF0F0L_V8materialAFyxGx_tcfC

    struct Relish<Material> {
      let material: Material
    }
    let _ = Relish(material: "pickles")
  }
}

struct Pepper {}
struct ChiliFlakes {}

// CHECK-LABEL: // nested_generics.eatDinnerGeneric<A, B where A: nested_generics.Pizza, B: nested_generics.HotDog, A.Topping: nested_generics.CuredMeat, B.Condiment == nested_generics.Deli<nested_generics.Pepper>.Mustard>(d: inout nested_generics.Lunch<A>.Dinner<B>, t: A.Topping, u: nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s15nested_generics16eatDinnerGeneric1d1t1uyAA5LunchV0D0Vyx_q_Gz_7ToppingQzAA4DeliC7MustardOyAA6PepperV_GtAA5PizzaRzAA6HotDogR_AA9CuredMeatALRQAS9CondimentRt_r0_lF : $@convention(thin) <T, U where T : Pizza, U : HotDog, T.Topping : CuredMeat, U.Condiment == Deli<Pepper>.Mustard> (@inout Lunch<T>.Dinner<U>, @in_guaranteed T.Topping, Deli<Pepper>.Mustard) -> ()

func eatDinnerGeneric<T, U>(d: inout Lunch<T>.Dinner<U>, t: T.Topping, u: U.Condiment) {
  // Method call
  _ = d.coolCombination(t: t, u: u)

  // Read a let, store into var
  d.leftovers = d.firstCourse

  // Read a var
  let _ = d.secondCourse

  // Call property of function type
  _ = d.transformation(d.leftovers)
}

// Overloading concrete function with different bound generic arguments in parent type

// CHECK-LABEL: // nested_generics.eatDinnerConcrete(d: inout nested_generics.Lunch<nested_generics.Pizzas<nested_generics.ChiliFlakes>.NewYork>.Dinner<nested_generics.HotDogs.American>, t: nested_generics.Deli<nested_generics.ChiliFlakes>.Pepperoni, u: nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s15nested_generics17eatDinnerConcrete1d1t1uyAA5LunchV0D0VyAA6PizzasV7NewYorkCyAA11ChiliFlakesV_G_AA7HotDogsC8AmericanVGz_AA4DeliC9PepperoniCyAO_GAW7MustardOyAA6PepperV_GtF : $@convention(thin) (@inout Lunch<Pizzas<ChiliFlakes>.NewYork>.Dinner<HotDogs.American>, @guaranteed Deli<ChiliFlakes>.Pepperoni, Deli<Pepper>.Mustard) -> ()

func eatDinnerConcrete(d: inout Lunch<Pizzas<ChiliFlakes>.NewYork>.Dinner<HotDogs.American>,
                       t: Deli<ChiliFlakes>.Pepperoni,
                       u: Deli<Pepper>.Mustard) {
  // Method call
  _ = d.coolCombination(t: t, u: u)

  // Read a let, store into var
  d.leftovers = d.firstCourse

  // Read a var
  let _ = d.secondCourse

  // Call property of function type
  _ = d.transformation(d.leftovers)
}

// CHECK-LABEL: // reabstraction thunk helper from @escaping @callee_guaranteed (@guaranteed nested_generics.Pizzas<nested_generics.ChiliFlakes>.NewYork) -> (@out nested_generics.HotDogs.American) to @escaping @callee_guaranteed (@guaranteed nested_generics.Pizzas<nested_generics.ChiliFlakes>.NewYork) -> (@unowned nested_generics.HotDogs.American)
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s15nested_generics6PizzasV7NewYorkCyAA11ChiliFlakesV_GAA7HotDogsC8AmericanVIeggr_AhLIeggd_TR : $@convention(thin) (@guaranteed Pizzas<ChiliFlakes>.NewYork, @guaranteed @callee_guaranteed (@guaranteed Pizzas<ChiliFlakes>.NewYork) -> @out HotDogs.American) -> HotDogs.American

// CHECK-LABEL: // nested_generics.eatDinnerConcrete(d: inout nested_generics.Lunch<nested_generics.Pizzas<nested_generics.Pepper>.NewYork>.Dinner<nested_generics.HotDogs.American>, t: nested_generics.Deli<nested_generics.Pepper>.Pepperoni, u: nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()
// CHECK-LABEL: sil hidden [ossa] @$s15nested_generics17eatDinnerConcrete1d1t1uyAA5LunchV0D0VyAA6PizzasV7NewYorkCyAA6PepperV_G_AA7HotDogsC8AmericanVGz_AA4DeliC9PepperoniCyAO_GAW7MustardOyAO_GtF : $@convention(thin) (@inout Lunch<Pizzas<Pepper>.NewYork>.Dinner<HotDogs.American>, @guaranteed Deli<Pepper>.Pepperoni, Deli<Pepper>.Mustard) -> ()

func eatDinnerConcrete(d: inout Lunch<Pizzas<Pepper>.NewYork>.Dinner<HotDogs.American>,
                       t: Deli<Pepper>.Pepperoni,
                       u: Deli<Pepper>.Mustard) {
  // Method call
  _ = d.coolCombination(t: t, u: u)

  // Read a let, store into var
  d.leftovers = d.firstCourse

  // Read a var
  let _ = d.secondCourse

  // Call property of function type
  _ = d.transformation(d.leftovers)
}

// CHECK-LABEL: // reabstraction thunk helper from @escaping @callee_guaranteed (@guaranteed nested_generics.Pizzas<nested_generics.Pepper>.NewYork) -> (@out nested_generics.HotDogs.American) to @escaping @callee_guaranteed (@guaranteed nested_generics.Pizzas<nested_generics.Pepper>.NewYork) -> (@unowned nested_generics.HotDogs.American)
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s15nested_generics6PizzasV7NewYorkCyAA6PepperV_GAA7HotDogsC8AmericanVIeggr_AhLIeggd_TR : $@convention(thin) (@guaranteed Pizzas<Pepper>.NewYork, @guaranteed @callee_guaranteed (@guaranteed Pizzas<Pepper>.NewYork) -> @out HotDogs.American) -> HotDogs.American

// CHECK-LABEL: // closure #1 (nested_generics.Pizzas<nested_generics.Pepper>.NewYork) -> nested_generics.HotDogs.American in nested_generics.calls() -> ()
// CHECK-LABEL: sil private [ossa] @$s15nested_generics5callsyyFAA7HotDogsC8AmericanVAA6PizzasV7NewYorkCyAA6PepperV_GcfU_ :

func calls() {

  let firstCourse = Pizzas<Pepper>.NewYork()
  let secondCourse = HotDogs.American()

  var dinner = Lunch<Pizzas<Pepper>.NewYork>.Dinner<HotDogs.American>(
      firstCourse: firstCourse,
      secondCourse: secondCourse,
      leftovers: firstCourse,
      transformation: { _ in HotDogs.American() })

  let topping = Deli<Pepper>.Pepperoni()

  let condiment1 = Deli<Pepper>.Mustard.Dijon
  let condiment2 = Deli<Pepper>.Mustard.DeliStyle(Pepper())

  eatDinnerGeneric(d: &dinner, t: topping, u: condiment1)
  eatDinnerConcrete(d: &dinner, t: topping, u: condiment2)

}

protocol ProtocolWithGenericRequirement {
  associatedtype T
  associatedtype U
  func method<V>(t: T, u: U, v: V) -> (T, U, V)
}

class OuterRing<T> {
  class InnerRing<U> : ProtocolWithGenericRequirement {
    func method<V>(t: T, u: U, v: V) -> (T, U, V) {
      return (t, u, v)
    }
  }
}

class SubclassOfInner<T, U> : OuterRing<T>.InnerRing<U> {
  override func method<V>(t: T, u: U, v: V) -> (T, U, V) {
    return super.method(t: t, u: u, v: v)
  }
}

// Reduced from some code in Doggie.  rdar://107642925
struct LocalGenericFunc<Element> {
  var address: UnsafeMutablePointer<Element>
  init(address: UnsafeMutablePointer<Element>) {
    self.address = address
  }

  mutating func foo() {
    func helper<S: Sequence>(_ newElements: S) where S.Element == Element {
      let buffer = address
    }
  }
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s15nested_generics9OuterRingC05InnerD0Cyx_qd__GAA30ProtocolWithGenericRequirementA2aGP6method1t1u1v1TQz_1UQzqd__tAN_APqd__tlFTW : $@convention(witness_method: ProtocolWithGenericRequirement) <τ_0_0><τ_1_0><τ_2_0> (@in_guaranteed τ_0_0, @in_guaranteed τ_1_0, @in_guaranteed τ_2_0, @in_guaranteed OuterRing<τ_0_0>.InnerRing<τ_1_0>) -> (@out τ_0_0, @out τ_1_0, @out τ_2_0) {
// CHECK: bb0([[T:%[0-9]+]] : $*τ_0_0, [[U:%[0-9]+]] : $*τ_1_0, [[V:%[0-9]+]] : $*τ_2_0, [[TOut:%[0-9]+]] : $*τ_0_0, [[UOut:%[0-9]+]] : $*τ_1_0, [[VOut:%[0-9]+]] : $*τ_2_0, [[SELF:%[0-9]+]] : $*OuterRing<τ_0_0>.InnerRing<τ_1_0>):
// CHECK:   [[SELF_COPY_VAL:%[0-9]+]] = load_borrow [[SELF]] : $*OuterRing<τ_0_0>.InnerRing<τ_1_0>
// CHECK:   [[METHOD:%[0-9]+]] = class_method [[SELF_COPY_VAL]] : $OuterRing<τ_0_0>.InnerRing<τ_1_0>, #OuterRing.InnerRing.method : <T><U><V> (OuterRing<T>.InnerRing<U>) -> (T, U, V) -> (T, U, V), $@convention(method) <τ_0_0><τ_1_0><τ_2_0> (@in_guaranteed τ_0_0, @in_guaranteed τ_1_0, @in_guaranteed τ_2_0, @guaranteed OuterRing<τ_0_0>.InnerRing<τ_1_0>) -> (@out τ_0_0, @out τ_1_0, @out τ_2_0)
// CHECK:   apply [[METHOD]]<τ_0_0, τ_1_0, τ_2_0>([[T]], [[U]], [[V]], [[TOut]], [[UOut]], [[VOut]], [[SELF_COPY_VAL]]) : $@convention(method) <τ_0_0><τ_1_0><τ_2_0> (@in_guaranteed τ_0_0, @in_guaranteed τ_1_0, @in_guaranteed τ_2_0, @guaranteed OuterRing<τ_0_0>.InnerRing<τ_1_0>) -> (@out τ_0_0, @out τ_1_0, @out τ_2_0)
// CHECK:   [[RESULT:%[0-9]+]] = tuple ()
// CHECK:   end_borrow [[SELF_COPY_VAL]]
// CHECK:   return [[RESULT]] : $()

// CHECK: sil_witness_table hidden <Spices> Deli<Spices>.Pepperoni: CuredMeat module nested_generics {
// CHECK: }

// CHECK: sil_witness_table hidden <Spices> Deli<Spices>.Sausage: CuredMeat module nested_generics {
// CHECK: }

// CHECK: sil_witness_table hidden <Spices> Deli<Spices>: CuredMeat module nested_generics {
// CHECK: }

// CHECK: sil_witness_table hidden <Spices> Pizzas<Spices>.NewYork: Pizza module nested_generics {
// CHECK:   associated_type Topping: Deli<Spices>.Pepperoni
// CHECK: }

// CHECK: sil_witness_table hidden <Spices> Pizzas<Spices>.DeepDish: Pizza module nested_generics {
// CHECK:   associated_type Topping: Deli<Spices>.Sausage
// CHECK: }

// CHECK: sil_witness_table hidden HotDogs.Bratwurst: HotDog module nested_generics {
// CHECK:   associated_type Condiment: Deli<Pepper>.Mustard
// CHECK: }

// CHECK: sil_witness_table hidden HotDogs.American: HotDog module nested_generics {
// CHECK:   associated_type Condiment: Deli<Pepper>.Mustard
// CHECK: }
