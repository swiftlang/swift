// RUN: %target-typecheck-verify-swift -disable-availability-checking -warn-redundant-requirements  -enable-experimental-implicit-some

protocol Eatery {
  func lunch()
  func dinner()
  func dessert()
}

protocol Shop {
  func coffee ()
}

// Access Return Type Constraint
protocol Cafe {
  func breakfast()
  func treats()
}

class SeasonalMenu : Cafe {
  func breakfast() {}
  func treats()  {}
}

func getCurrentMenu() -> some Cafe {
  return SeasonalMenu()
}

var cafe = getCurrentMenu()
cafe.breakfast()
cafe.treats()

// Type alias + protocols
typealias Snack = Shop & Cafe
typealias Meal = Eatery

struct CoffeeShop: Snack {
  func coffee(){ }
  func breakfast() { }
  func treats(){ }
}

class Best: Eatery {
  func lunch() { };
  func dinner() { }
  func dessert() { }
}

class TopTier: Eatery {
  func lunch() { };
  func dinner() { }
  func dessert() { }
}

func find() -> Eatery { } // expected-error {{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}

func find() -> AnyObject {
  return CoffeeShop() // expected-error {{return expression of type 'CoffeeShop' expected to be an instance of a class or class-constrained type}}
}

func find() -> Any {
  return CoffeeShop()
}

// tuple types
func highestRated() -> (Eatery, Eatery) { } // expected-error {{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}

// type alias
func inspect( _ snack: some Snack) -> some Snack {
  return CoffeeShop();
}
// tuple type alias
func highestRated() -> (some Snack, some Snack) { } // expected-error {{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}

// TO-DO: Fix type alias for plain protocols; resolves as an existential type
func list(_: (Meal, Meal)) -> (Meal, Meal){ }
func find() -> Snack { }

// opaque compostion types
func search() -> Shop & Cafe { } // expected-error {{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}

// associated type constraint
protocol Basket {
  associatedtype Fruit
  associatedtype MiniBasket: Basket where MiniBasket.Fruit == Fruit
  
  var fruit: [Fruit] { get set }
  var minifruitbasket: [MiniBasket] { get set }
}

struct MyFruit : Basket {
  var fruit: [String]
  var minifruitbasket: [OtherFruit]
}

struct OtherFruit : Basket {
  var fruit: [String]
  var minifruitbasket: [OtherFruit]
}

func eat(_ myfruit: inout Basket) -> Basket {
  myfruit.fruit.removeLast()
  myfruit.minifruitbasket.removeLast()
  return myfruit
}

