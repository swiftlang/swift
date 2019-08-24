public protocol Bed {
  func squiggle()
}

public protocol Outfit {
  var size: Int { get }
}

public protocol Eater {
#if BEFORE
  func eat()
  func poop()
#else
  func poop()
  func eat()
#endif
}

public protocol Wiggler {
#if BEFORE
  func wiggle()
  func cry()
#else
  func cry()
  func wiggle()
#endif
}

#if BEFORE

public protocol Baby : Eater, Wiggler {
  associatedtype Bassinet : Bed
  associatedtype Onesie : Outfit

  var outfitSize: Int { get }

  func sleep(in: Bassinet)
  func wear(outfit: Onesie)
}

#else

public protocol Baby : Wiggler, Eater {
  associatedtype Onesie : Outfit
  associatedtype Bassinet : Bed

  var outfitSize: Int { get }

  func wear(outfit: Onesie)
  func sleep(in: Bassinet)
}

#endif

public protocol Adoring {
  func adore() -> String
}

public func goodDay<B : Baby>(for baby: B,
                              sleepingIn bed: B.Bassinet,
                              wearing outfit: B.Onesie) {
  if baby.outfitSize != outfit.size {
    fatalError("I grew too much!")
  }

  baby.wear(outfit: outfit)
  baby.sleep(in: bed)
  baby.poop()
  baby.sleep(in: bed)
  baby.eat()
  baby.sleep(in: bed)
  baby.wiggle()
}
