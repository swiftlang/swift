public protocol Animal {
  associatedtype AnimalSnackType : AnimalSnack
  func snack(on: AnimalSnackType)
}

public protocol AnimalSnack {
  associatedtype EatWith
  func eat(with: EatWith)
}

extension AnimalSnack where EatWith : Animal {}

public protocol FurryAnimal : Animal {
    associatedtype Fangs : Animal
    func bite(with: Fangs)
}

extension FurryAnimal {
  public func snack(on: FurryAnimalSnack<Self>) {}
}

public struct FurryAnimalSnack<T : FurryAnimal> : AnimalSnack {
  public func eat(with: T) {}
}
