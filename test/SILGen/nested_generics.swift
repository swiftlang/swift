// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen -enable-experimental-nested-generic-types -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-sil -enable-experimental-nested-generic-types -parse-as-library %s > /dev/null
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-sil -O -enable-experimental-nested-generic-types -parse-as-library %s > /dev/null
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-ir -enable-experimental-nested-generic-types -parse-as-library %s > /dev/null

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

// CHECK-LABEL: // nested_generics.Lunch.Dinner.coolCombination (t : A.Topping, u : A1.Condiment) -> ()
// CHECK-LABEL: sil hidden @_TFVV15nested_generics5Lunch6Dinner15coolCombinationfT1twx7Topping1uwd__9Condiment_T_ : $@convention(method) <T where T : Pizza, T.Topping : CuredMeat><U where U : HotDog, U.Condiment == Deli<Pepper>.Mustard> (@in T.Topping, Deli<Pepper>.Mustard, @in_guaranteed Lunch<T>.Dinner<U>) -> ()

// CHECK-LABEL: // nested_generics.Lunch.Dinner.(coolCombination (t : A.Topping, u : A1.Condiment) -> ()).(nestedGeneric #1) <A><A1><A2, B2 where A: nested_generics.Pizza, A1: nested_generics.HotDog, A.Topping: nested_generics.CuredMeat, A1.Condiment == nested_generics.Deli<nested_generics.Pepper>.Mustard> (x : A2, y : B2) -> (A2, B2)
// CHECK-LABEL: sil shared @_TFFVV15nested_generics5Lunch6Dinner15coolCombinationFT1twx7Topping1uwd__9Condiment_T_L_13nestedGenericu__0_RxS_5Pizzad__S_6HotDogwxS2_S_9CuredMeatwd__S3_zGOCS_4Deli7MustardVS_6Pepper__rFT1xqd0__1yqd0_0__Tqd0__qd0_0__ : $@convention(thin) <T where T : Pizza, T.Topping : CuredMeat><U where U : HotDog, U.Condiment == Deli<Pepper>.Mustard><X, Y> (@in X, @in Y) -> (@out X, @out Y)

// CHECK-LABEL: // nested_generics.Lunch.Dinner.init (firstCourse : A, secondCourse : Swift.Optional<A1>, leftovers : A, transformation : (A) -> A1) -> nested_generics.Lunch<A>.Dinner<A1>
// CHECK-LABEL: sil hidden @_TFVV15nested_generics5Lunch6DinnerCfT11firstCoursex12secondCourseGSqqd___9leftoversx14transformationFxqd___GS1_x_qd___ : $@convention(method) <T where T : Pizza, T.Topping : CuredMeat><U where U : HotDog, U.Condiment == Deli<Pepper>.Mustard> (@owned T, @in Optional<U>, @owned T, @owned @callee_owned (@owned T) -> @out U, @thin Lunch<T>.Dinner<U>.Type) -> @out Lunch<T>.Dinner<U>

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
// CHECK-LABEL: sil hidden @_TFCC15nested_generics4Deli9PepperonicfT_GS1_x__ : $@convention(method) <Spices> (@owned Deli<Spices>.Pepperoni) -> @owned Deli<Spices>.Pepperoni

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
    // CHECK-LABEL: sil shared @_TFVFE15nested_genericsSS3fooFT_T_L_6CheeseCfT8materialx_GS0_x_
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
    // CHECK-LABEL: sil shared @_TFVFC15nested_generics7HotDogs11applyRelishFT_T_L_6RelishCfT8materialx_GS1_x_

    struct Relish<Material> {
      let material: Material
    }
    let r = Relish(material: "pickles")
  }
}

struct Pepper {}
struct ChiliFlakes {}

// CHECK-LABEL: // nested_generics.eatDinnerGeneric <A, B where A: nested_generics.Pizza, B: nested_generics.HotDog, A.Topping: nested_generics.CuredMeat, B.Condiment == nested_generics.Deli<nested_generics.Pepper>.Mustard> (d : inout nested_generics.Lunch<A>.Dinner<B>, t : A.Topping, u : B.Condiment) -> ()
// CHECK-LABEL: sil hidden @_TF15nested_generics16eatDinnerGenericu0_RxS_5Pizza_S_6HotDogwx7ToppingS_9CuredMeatw_9CondimentzGOCS_4Deli7MustardVS_6Pepper__rFT1dRGVVS_5Lunch6Dinnerx_q__1twxS2_1uw_S4__T_ : $@convention(thin) <T, U where T : Pizza, U : HotDog, T.Topping : CuredMeat, U.Condiment == Deli<Pepper>.Mustard> (@inout Lunch<T>.Dinner<U>, @in T.Topping, Deli<Pepper>.Mustard) -> ()

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
// CHECK-LABEL: sil hidden @_TF15nested_generics17eatDinnerConcreteFT1dRGVVS_5Lunch6DinnerGCVS_6Pizzas7NewYorkVS_11ChiliFlakes___VCS_7HotDogs8American_1tGCCS_4Deli9PepperoniS4___1uGOS7_7MustardVS_6Pepper___T_ : $@convention(thin) (@inout Lunch<Pizzas<ChiliFlakes>.NewYork>.Dinner<HotDogs.American>, @owned Deli<ChiliFlakes>.Pepperoni, Deli<Pepper>.Mustard) -> ()

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
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_oGCV15nested_generics6Pizzas7NewYorkVS_11ChiliFlakes___iVCS_7HotDogs8American_XFo_oGS1_S2____dS4__ : $@convention(thin) (@owned Pizzas<ChiliFlakes>.NewYork, @owned @callee_owned (@owned Pizzas<ChiliFlakes>.NewYork) -> @out HotDogs.American) -> HotDogs.American

// CHECK-LABEL: // nested_generics.eatDinnerConcrete (d : inout nested_generics.Lunch<nested_generics.Pizzas<nested_generics.Pepper>.NewYork>.Dinner<nested_generics.HotDogs.American>, t : nested_generics.Deli<nested_generics.Pepper>.Pepperoni, u : nested_generics.Deli<nested_generics.Pepper>.Mustard) -> ()
// CHECK-LABEL: sil hidden @_TF15nested_generics17eatDinnerConcreteFT1dRGVVS_5Lunch6DinnerGCVS_6Pizzas7NewYorkVS_6Pepper___VCS_7HotDogs8American_1tGCCS_4Deli9PepperoniS4___1uGOS7_7MustardS4____T_ : $@convention(thin) (@inout Lunch<Pizzas<Pepper>.NewYork>.Dinner<HotDogs.American>, @owned Deli<Pepper>.Pepperoni, Deli<Pepper>.Mustard) -> ()

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
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_oGCV15nested_generics6Pizzas7NewYorkVS_6Pepper___iVCS_7HotDogs8American_XFo_oGS1_S2____dS4__ : $@convention(thin) (@owned Pizzas<Pepper>.NewYork, @owned @callee_owned (@owned Pizzas<Pepper>.NewYork) -> @out HotDogs.American) -> HotDogs.American

// CHECK-LABEL: // nested_generics.(calls () -> ()).(closure #1)
// CHECK-LABEL: sil shared @_TFF15nested_generics5callsFT_T_U_FGCVS_6Pizzas7NewYorkVS_6Pepper__VCS_7HotDogs8American : $@convention(thin) (@owned Pizzas<Pepper>.NewYork) -> HotDogs.American

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

// CHECK-LABEL: // reabstraction thunk helper from @callee_owned (@owned nested_generics.Pizzas<nested_generics.Pepper>.NewYork) -> (@unowned nested_generics.HotDogs.American) to @callee_owned (@owned nested_generics.Pizzas<nested_generics.Pepper>.NewYork) -> (@out nested_generics.HotDogs.American)
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_oGCV15nested_generics6Pizzas7NewYorkVS_6Pepper___dVCS_7HotDogs8American_XFo_oGS1_S2____iS4__ : $@convention(thin) (@owned Pizzas<Pepper>.NewYork, @owned @callee_owned (@owned Pizzas<Pepper>.NewYork) -> HotDogs.American) -> @out HotDogs.American

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
