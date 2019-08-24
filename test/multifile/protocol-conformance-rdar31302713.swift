// RUN: %target-swift-frontend -emit-ir -primary-file %s %S/Inputs/protocol-conformance/rdar31302713.swift -module-name animal_snack

public struct FunctionalFurryAnimal<Fangs : Animal> : FurryAnimal
    where Fangs.AnimalSnackType.EatWith == Fangs {
  public func bite(with: Fangs) {}
}
