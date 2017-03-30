// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-sil -parse-as-library %s > /dev/null
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-sil -O -parse-as-library %s > /dev/null
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-ir -parse-as-library %s > /dev/null

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

struct Lunch<T : Pizza where T.Topping : CuredMeat> {
  struct Dinner<U : HotDog where U.Condiment == Deli<Pepper>.Mustard> {
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

// CHECK-LABEL: // nested_generics.Lunch.Dinner.coolCombination (t : A.Topping, u : nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()
// CHECK-LABEL: sil hidden @_T015nested_generics5LunchV6DinnerV15coolCombinationy7ToppingQz1t_AA4DeliC7MustardOyAA6PepperV_G1utF : $@convention(method) <T where T : Pizza, T.Topping : CuredMeat><U where U : HotDog, U.Condiment == Deli<Pepper>.Mustard> (@in T.Topping, Deli<Pepper>.Mustard, @in_guaranteed Lunch<T>.Dinner<U>) -> ()

// CHECK-LABEL: // nested_generics.Lunch.Dinner.(coolCombination (t : A.Topping, u : nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()).(nestedGeneric #1) <A><A1><A2, B2 where A: nested_generics.Pizza, A1: nested_generics.HotDog, A.Topping: nested_generics.CuredMeat, A1.Condiment == nested_generics.Deli<nested_generics.Pepper>.Mustard> (x : A2, y : B2) -> (A2, B2)
// CHECK-LABEL: sil shared @_T015nested_generics5LunchV6DinnerV15coolCombinationy7ToppingQz1t_AA4DeliC7MustardOyAA6PepperV_G1utF0A7GenericL_qd0___qd0_0_tqd0__1x_qd0_0_1ytAA5PizzaRzAA6HotDogRd__AA9CuredMeatAHRQAP9CondimentRtd__r__0_lF : $@convention(thin) <T where T : Pizza, T.Topping : CuredMeat><U where U : HotDog, U.Condiment == Deli<Pepper>.Mustard><X, Y> (@in X, @in Y) -> (@out X, @out Y)

// CHECK-LABEL: // nested_generics.Lunch.Dinner.init (firstCourse : A, secondCourse : Swift.Optional<A1>, leftovers : A, transformation : (A) -> A1) -> nested_generics.Lunch<A>.Dinner<A1>
// CHECK-LABEL: sil hidden @_T015nested_generics5LunchV6DinnerVAEyx_qd__Gx11firstCourse_qd__Sg06secondF0x9leftoversqd__xc14transformationtcfC : $@convention(method) <T where T : Pizza, T.Topping : CuredMeat><U where U : HotDog, U.Condiment == Deli<Pepper>.Mustard> (@owned T, @in Optional<U>, @owned T, @owned @callee_owned (@owned T) -> @out U, @thin Lunch<T>.Dinner<U>.Type) -> @out Lunch<T>.Dinner<U>

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

// CHECK-LABEL: // nested_generics.Deli.Pepperoni.init () -> nested_generics.Deli<A>.Pepperoni
// CHECK-LABEL: sil hidden @_T015nested_generics4DeliC9PepperoniCAEyx_Gycfc : $@convention(method) <Spices> (@owned Deli<Spices>.Pepperoni) -> @owned Deli<Spices>.Pepperoni

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
    // CHECK-LABEL: // (extension in nested_generics):Swift.String.(foo () -> ()).(Cheese #1).init (material : A) -> (extension in nested_generics):Swift.String.(foo () -> ()).(Cheese #1)<A>
    // CHECK-LABEL: sil shared @_T0SS15nested_genericsE3fooyyF6CheeseL_VADyxGx8material_tcfC
    struct Cheese<Milk> {
      let material: Milk
    }
    let r = Cheese(material: "cow")
  }
}

// Local type in extension of type in same module
extension HotDogs {
  func applyRelish() {
    // CHECK-LABEL: // nested_generics.HotDogs.(applyRelish () -> ()).(Relish #1).init (material : A) -> nested_generics.HotDogs.(applyRelish () -> ()).(Relish #1)<A>
    // CHECK-LABEL: sil shared @_T015nested_generics7HotDogsC11applyRelishyyF0F0L_VAFyxGx8material_tcfC

    struct Relish<Material> {
      let material: Material
    }
    let r = Relish(material: "pickles")
  }
}

struct Pepper {}
struct ChiliFlakes {}

// CHECK-LABEL: // nested_generics.eatDinnerGeneric <A, B where A: nested_generics.Pizza, B: nested_generics.HotDog, A.Topping: nested_generics.CuredMeat, B.Condiment == nested_generics.Deli<nested_generics.Pepper>.Mustard> (d : inout nested_generics.Lunch<A>.Dinner<B>, t : A.Topping, u : nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()
// CHECK-LABEL: sil hidden @_T015nested_generics16eatDinnerGenericyAA5LunchV0D0Vyx_q_Gz1d_7ToppingQz1tAA4DeliC7MustardOyAA6PepperV_G1utAA5PizzaRzAA6HotDogR_AA9CuredMeatAJRQAR9CondimentRt_r0_lF : $@convention(thin) <T, U where T : Pizza, U : HotDog, T.Topping : CuredMeat, U.Condiment == Deli<Pepper>.Mustard> (@inout Lunch<T>.Dinner<U>, @in T.Topping, Deli<Pepper>.Mustard) -> ()

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

// CHECK-LABEL: // nested_generics.eatDinnerConcrete (d : inout nested_generics.Lunch<nested_generics.Pizzas<nested_generics.ChiliFlakes>.NewYork>.Dinner<nested_generics.HotDogs.American>, t : nested_generics.Deli<nested_generics.ChiliFlakes>.Pepperoni, u : nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()
// CHECK-LABEL: sil hidden @_T015nested_generics17eatDinnerConcreteyAA5LunchV0D0VyAA6PizzasV7NewYorkCyAA11ChiliFlakesV_G_AA7HotDogsC8AmericanVGz1d_AA4DeliC9PepperoniCyAL_G1tAU7MustardOyAA6PepperV_G1utF : $@convention(thin) (@inout Lunch<Pizzas<ChiliFlakes>.NewYork>.Dinner<HotDogs.American>, @owned Deli<ChiliFlakes>.Pepperoni, Deli<Pepper>.Mustard) -> ()

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

// CHECK-LABEL: // reabstraction thunk helper from @callee_owned (@owned nested_generics.Pizzas<nested_generics.ChiliFlakes>.NewYork) -> (@out nested_generics.HotDogs.American) to @callee_owned (@owned nested_generics.Pizzas<nested_generics.ChiliFlakes>.NewYork) -> (@unowned nested_generics.HotDogs.American)
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @_T015nested_generics6PizzasV7NewYorkCyAA11ChiliFlakesV_GAA7HotDogsC8AmericanVIxxr_AhLIxxd_TR : $@convention(thin) (@owned Pizzas<ChiliFlakes>.NewYork, @owned @callee_owned (@owned Pizzas<ChiliFlakes>.NewYork) -> @out HotDogs.American) -> HotDogs.American

// CHECK-LABEL: // nested_generics.eatDinnerConcrete (d : inout nested_generics.Lunch<nested_generics.Pizzas<nested_generics.Pepper>.NewYork>.Dinner<nested_generics.HotDogs.American>, t : nested_generics.Deli<nested_generics.Pepper>.Pepperoni, u : nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()
// CHECK-LABEL: sil hidden @_T015nested_generics17eatDinnerConcreteyAA5LunchV0D0VyAA6PizzasV7NewYorkCyAA6PepperV_G_AA7HotDogsC8AmericanVGz1d_AA4DeliC9PepperoniCyAL_G1tAU7MustardOyAL_G1utF : $@convention(thin) (@inout Lunch<Pizzas<Pepper>.NewYork>.Dinner<HotDogs.American>, @owned Deli<Pepper>.Pepperoni, Deli<Pepper>.Mustard) -> ()

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

// CHECK-LABEL: // reabstraction thunk helper from @callee_owned (@owned nested_generics.Pizzas<nested_generics.Pepper>.NewYork) -> (@out nested_generics.HotDogs.American) to @callee_owned (@owned nested_generics.Pizzas<nested_generics.Pepper>.NewYork) -> (@unowned nested_generics.HotDogs.American)
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @_T015nested_generics6PizzasV7NewYorkCyAA6PepperV_GAA7HotDogsC8AmericanVIxxr_AhLIxxd_TR : $@convention(thin) (@owned Pizzas<Pepper>.NewYork, @owned @callee_owned (@owned Pizzas<Pepper>.NewYork) -> @out HotDogs.American) -> HotDogs.American

// CHECK-LABEL: // nested_generics.(calls () -> ()).(closure #1)
// CHECK-LABEL: sil shared @_T015nested_generics5callsyyFAA7HotDogsC8AmericanVAA6PizzasV7NewYorkCyAA6PepperV_GcfU_ : $@convention(thin) (@owned Pizzas<Pepper>.NewYork) -> HotDogs.American

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

// CHECK-LABEL: // reabstraction thunk helper from @callee_owned (@owned nested_generics.Pizzas<nested_generics.Pepper>.NewYork) -> (@unowned nested_generics.HotDogs.American) to @callee_owned (@owned nested_generics.Pizzas<nested_generics.Pepper>.NewYork) -> (@out nested_generics.HotDogs.American)
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @_T015nested_generics6PizzasV7NewYorkCyAA6PepperV_GAA7HotDogsC8AmericanVIxxd_AhLIxxr_TR : $@convention(thin) (@owned Pizzas<Pepper>.NewYork, @owned @callee_owned (@owned Pizzas<Pepper>.NewYork) -> HotDogs.American) -> @out HotDogs.American

// CHECK-LABEL: sil hidden [transparent] [thunk] @_T015nested_generics9OuterRingC05InnerD0Cyx_qd__GAA30ProtocolWithGenericRequirementAAr__lAaGP6method1TQz_1UQzqd__tAK1t_AM1uqd__1vtlFTW : $@convention(witness_method) <τ_0_0><τ_1_0><τ_2_0> (@in τ_0_0, @in τ_1_0, @in τ_2_0, @in_guaranteed OuterRing<τ_0_0>.InnerRing<τ_1_0>) -> (@out τ_0_0, @out τ_1_0, @out τ_2_0) {
// CHECK: bb0([[T:%[0-9]+]] : $*τ_0_0, [[U:%[0-9]+]] : $*τ_1_0, [[V:%[0-9]+]] : $*τ_2_0, [[TOut:%[0-9]+]] : $*τ_0_0, [[UOut:%[0-9]+]] : $*τ_1_0, [[VOut:%[0-9]+]] : $*τ_2_0, [[SELF:%[0-9]+]] : $*OuterRing<τ_0_0>.InnerRing<τ_1_0>):
// CHECK:   [[SELF_COPY:%[0-9]+]] = alloc_stack $OuterRing<τ_0_0>.InnerRing<τ_1_0>
// CHECK:   copy_addr [[SELF]] to [initialization] [[SELF_COPY]] : $*OuterRing<τ_0_0>.InnerRing<τ_1_0>
// CHECK:   [[SELF_COPY_VAL:%[0-9]+]] = load [take] [[SELF_COPY]] : $*OuterRing<τ_0_0>.InnerRing<τ_1_0>
// CHECK:   [[BORROWED_SELF_COPY_VAL:%.*]] = begin_borrow [[SELF_COPY_VAL]]
// CHECK:   [[METHOD:%[0-9]+]] = class_method [[BORROWED_SELF_COPY_VAL]] : $OuterRing<τ_0_0>.InnerRing<τ_1_0>, #OuterRing.InnerRing.method!1 : <T><U><V> (OuterRing<T>.InnerRing<U>) -> (T, U, V) -> (T, U, V), $@convention(method) <τ_0_0><τ_1_0><τ_2_0> (@in τ_0_0, @in τ_1_0, @in τ_2_0, @guaranteed OuterRing<τ_0_0>.InnerRing<τ_1_0>) -> (@out τ_0_0, @out τ_1_0, @out τ_2_0)
// CHECK:   apply [[METHOD]]<τ_0_0, τ_1_0, τ_2_0>([[T]], [[U]], [[V]], [[TOut]], [[UOut]], [[VOut]], [[BORROWED_SELF_COPY_VAL]]) : $@convention(method) <τ_0_0><τ_1_0><τ_2_0> (@in τ_0_0, @in τ_1_0, @in τ_2_0, @guaranteed OuterRing<τ_0_0>.InnerRing<τ_1_0>) -> (@out τ_0_0, @out τ_1_0, @out τ_2_0)
// CHECK:   [[RESULT:%[0-9]+]] = tuple ()
// CHECK:   end_borrow [[BORROWED_SELF_COPY_VAL]] from [[SELF_COPY_VAL]]
// CHECK:   destroy_value [[SELF_COPY_VAL]] : $OuterRing<τ_0_0>.InnerRing<τ_1_0>
// CHECK:   dealloc_stack [[SELF_COPY]] : $*OuterRing<τ_0_0>.InnerRing<τ_1_0>
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
