// RUN: %target-typecheck-verify-swift -disable-availability-checking -warn-redundant-requirements  -enable-experimental-implicit-some

// Access Return Type Constraint
protocol Cafe {
  func breakfast()
  func treats()
}

class SeasonalMenu : Cafe {
  func breakfast() {}
  func treats()  {}
}

func getCurrentMenu () -> some Cafe {
  return SeasonalMenu()
}

var cafe = getCurrentMenu()
cafe.breakfast()
cafe.treats()

// Type alias + custom protocols
typealias Snack = Shop & Cafe
typealias Meal = Eatery

struct CoffeeBar: Snack {
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

func list(_: Snack) {}

func find( _: Snack) -> Snack { } // expected-error {{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}

// tuple types
func list(_: (Meal, Meal)) {}

func highestRated() -> (Eatery, Eatery) {
   return (Best(), TopTier())
}

func highestRated() -> (some Snack, some Snack) { } // expected-error {{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}

// opaque compostion types
func find() -> Shop & Cafe {
  return CoffeeBar()
}

func find() -> AnyObject {
  return CoffeeBar() //  expected-error {{Return expression of type 'CoffeeBar' expected to be an instance of a class or class-constrained type}} 
}

func find() -> Any {
  return CoffeeBar()
}

// protocol with associated type constraint
protocol Basket {
  associatedtype Fruit
  associatedtype MiniBasket: Basket where MiniBasket.Fruit == Fruit
  
  var fruits: [Fruit] { get set }
  var minibaskets: [MiniBasket] { get set }
}

struct MyFruit : Basket {
  var fruits: [String]
  var minibaskets: [FruitforFriend]
}

struct FruitforFriend : Basket {
  var fruits: [String]
  var minibaskets: [FruitforFriend]
}

func eat(_ myfruit: inout Basket) -> Basket {
  myfruit.fruits.removeLast()
  myfruit.minibaskets.removeLast()
  return myfruit
}

