// RUN: %target-typecheck-verify-swift -swift-version 4

// https://bugs.swift.org/browse/SR-1660

enum DayOfTheWeek : Int {
  case monday = 0
  case `inout` = 1
  case `init` = 2
  case friday = 3
  case tuesday = 4
}

let _: DayOfTheWeek = DayOfTheWeek.init

let _: DayOfTheWeek = DayOfTheWeek.`init`

func match(_ d: DayOfTheWeek) {
  switch d {
  case .monday: break
  case .`inout`: break
  case .`init`: break
  case .friday: break
  case .tuesday: break
  }
}

enum Fox {
  case `init`(Int)
  
  init() {
    self = .`init`(10)
  }
}

let _: Fox = Fox(10)
// expected-error@-1 {{argument passed to call that takes no arguments}}

let _: () -> Fox = Fox.init
let _: (Int) -> Fox = Fox.`init`

func match(_ f: Fox) {
  switch f {
  case .`init`(let n): _ = n
  }
}
