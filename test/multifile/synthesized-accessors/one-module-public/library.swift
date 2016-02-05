// RUN: true

public struct FishAndChips {
  public var costPounds: Float
  public var costEuros: Float {
    get {
      return costPounds * 0.77
    }
    set {
      costPounds = newValue / 0.77
    }
  }
  public var costDollars: Float {
    get {
      return costPounds * 0.92
    }
    nonmutating set {}
  }
}
