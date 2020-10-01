// RUN: %empty-directory(%t)

// For convenience, this file includes the three different "files" used in this
// test. It selects one with -DCoreDishwasher, -DDishwasherKit, or neither.

// RUN: %target-swift-frontend -emit-module %s -DCoreDishwasher -module-name CoreDishwasher -o %t/CoreDishwasher -emit-module-path %t/CoreDishwasher.swiftmodule -I %t
// RUN: %target-swift-frontend -emit-module %s -DDishwasherKit -module-name DishwasherKit -o %t/DishwasherKit -emit-module-path %t/DishwasherKit.swiftmodule -enable-library-evolution -I %t
// RUN: %target-typecheck-verify-swift -I %t

#if CoreDishwasher

  public struct SpimsterWicket {
    public init() {}
  }

#elseif DishwasherKit

  @_implementationOnly import CoreDishwasher

  open class Dishwasher {
    public init() {}
    
    var wicket = SpimsterWicket()
    
    open var modelName: String { "Dishwasher" }
  }

  open class NoUserServiceablePartsInside {
    public init() {}
    internal init(wicket: SpimsterWicket) {}
    public convenience init(inconvenient: ()) {
      self.init()
    }
  }

#else

  import DishwasherKit

  class FancyDishwasher: Dishwasher {
    override var modelName: String { "Fancy \(super.modelName)" }
  }

  class VoidedWarranty: NoUserServiceablePartsInside {
    override init() { super.init() }
  }

  // FIXME: This diagnostic should be better, but it matches what we're already
  // doing for disallowed convenience inits.
  let notAllowed = VoidedWarranty(inconvenient: ()) // expected-error{{argument passed to call that takes no arguments}}

#endif
