// RUN: %target-swift-frontend %s -emit-sil -verify \
// RUN:   -enable-experimental-feature ManualOwnership \
// RUN:   -Wwarning DynamicExclusivity

// REQUIRES: swift_feature_ManualOwnership

protocol NutritionInfo {
  var calories: Int { get }
}

public class Food: NutritionInfo {
  var calories = 0
}

public class Donut : Food {

  let next: Donut? = nil

  override init() {
    super.init()
    self.calories = 100 // expected-warning {{exclusive access here will be checked at runtime}}
  }

  convenience init(calories c: Int) {
    self.init()
    self.calories = c // expected-warning {{exclusive access here will be checked at runtime}}
  }
}

extension Int { func greaterThanZero() -> Bool { self > 0 } }

var expectedCalories: Array<Int> = [120, 203, 1502]

func accessGlobal_map() -> Array<Donut> {
  return expectedCalories.map(Donut.init(calories:)) // expected-warning {{accessing 'expectedCalories' here may incur runtime exclusivity check, because it involves a global variable}}
}

func accessGlobal_member() -> Int {
  return expectedCalories.count // expected-warning {{accessing 'expectedCalories' here may incur runtime exclusivity check, because it involves a global variable}}
}

@_noManualOwnership
func accessGlobal_member_DISABLED() -> Int {
  return expectedCalories.count
}

var globalDonut: Donut = Donut()

func accessGlobalClass() {
  let x = globalDonut.calories // expected-warning {{accessing 'globalDonut' here may incur runtime exclusivity check, because it involves a global variable}}
                               // expected-warning@-1 {{exclusive access here will be checked at runtime}}

  expectedCalories.append(x)   // expected-warning {{accessing 'expectedCalories' here may incur runtime exclusivity check, because it involves a global variable}}
}

func accessClassSimple(_ d2: Donut) -> Int {
  let d1 = Donut()
  return d1.calories // expected-warning {{exclusive access here will be checked at runtime}}
       + d2.calories // expected-warning {{exclusive access here will be checked at runtime}}
}

func accessClassParam_chain(_ donut: Donut) -> Int {
  return donut.next?.next?.calories ?? Int.max
  // expected-warning@-1 {{exclusive access here will be checked at runtime}}
}

func accessClassLocal_chain() -> Int {
  let donut = Donut()
  return donut.next?.next?.calories ?? Int.max
  // expected-warning@-1 {{exclusive access here will be checked at runtime}}
}
