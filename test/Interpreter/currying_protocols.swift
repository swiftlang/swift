// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

enum Medal {
  case Bronze, Silver, Gold
}

struct Steroids {}

protocol Gymnast {
  func backflip(_ angle: Double) -> Self
  func compete() -> (Medal) -> Self
  func scandal() -> (Steroids) -> ()
  static func currentYear() -> Int
}

final class Archimedes : Gymnast {
  func backflip(_ angle: Double) -> Self {
    print(angle >= 360 ? "Not in a thousand years" : "Ok")
    return self
  }

  func compete() -> (Medal) -> Archimedes {
    return { (m: Medal) in
      print(m == .Gold ? "We won gold!" : "Try again next time")
      return Archimedes()
    }
  }

  func scandal() -> (Steroids) -> () {
    return { s in print("Archimedes don't do that") }
  }

  static func currentYear() -> Int {
    return -287
  }
}

////
// Protocol-constrained archetype
////

func genericOlympicGames<T: Gymnast>(_ g1: T) -> T {
  let f1: (T) -> (Double) -> T = T.backflip
  let f2: (Double) -> T = f1(g1)
  let g2: T = f2(180)

  let f3: (Double) -> T = g2.backflip
  let g3: T = f3(360)

  let f4: (T) -> () -> (Medal) -> T = T.compete
  let f5: () -> (Medal) -> T = f4(g3)
  let f6: (Medal) -> T = f5()
  let g4: T = f6(Medal.Silver)

  let f7: () -> (Medal) -> T = g4.compete
  let f8: (Medal) -> T = f7()
  let g5: T = f8(Medal.Gold)

  let f9: (T) -> () -> (Steroids) -> () = T.scandal
  let f10: () -> (Steroids) -> () = f9(g5)
  let f11: (Steroids) -> () = f10()
  f11(Steroids())

  let f12: () -> (Steroids) -> () = g5.scandal
  let f13: (Steroids) -> () = f12()
  f13(Steroids())

  let f14: () -> Int = T.currentYear
  print(f14())

  let metatype: T.Type = T.self
  let f15: () -> Int = metatype.currentYear
  print(f15())

  return g5
}

genericOlympicGames(Archimedes())

// CHECK: Ok
// CHECK: Not in a thousand years
// CHECK: Try again next time
// CHECK: We won gold!
// CHECK: Archimedes don't do that
// CHECK: Archimedes don't do that
// CHECK: -287
// CHECK: -287

////
// Existential
////

func olympicGames(_ g1: Gymnast) -> Gymnast {
  // FIXME -- <rdar://problem/21391055>
#if false
  let f1: (Double) -> Gymnast = g1.backflip
  let g2: Gymnast = f1(180)

  let f2: (Medal) -> Gymnast = g2.compete
  let g4: Gymnast = f2()(Medal.Gold)
#endif

  let f3: () -> (Steroids) -> () = g1.scandal
  let f4: (Steroids) -> () = f3()
  f4(Steroids())

  let f5: () -> Int = type(of: g1).currentYear
  print(f5())

  return g1
}

olympicGames(Archimedes())

// CHECK: Archimedes don't do that
// CHECK: -287
