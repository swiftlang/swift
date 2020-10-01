// RUN: %empty-directory(%t)

// For convenience, this file includes the three different "files" used in this
// test. It selects one with -DCoreDishwasher, -DDishwasherKit, or neither.

// RUN: %target-swift-frontend -emit-module %s -DCoreDishwasher -module-name CoreDishwasher -o %t/CoreDishwasher -emit-module-path %t/CoreDishwasher.swiftmodule -I %t
// RUN: %target-swift-frontend -emit-module %s -DDishwasherKit -module-name DishwasherKit -o %t/DishwasherKit -emit-module-path %t/DishwasherKit.swiftmodule -enable-library-evolution -I %t
// RUN: %target-swift-frontend -emit-silgen -I %t %s

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

#else

  import DishwasherKit

  open class FancyDishwasher: Dishwasher {
    open override var modelName: String { "Fancy \(super.modelName)" }
  }

#endif
