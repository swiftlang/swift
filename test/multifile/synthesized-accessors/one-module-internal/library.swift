// RUN: true

struct FishAndChips {
  var costPounds: Float
  var costEuros: Float {
    get {
      return costPounds * 0.77
    }
    set {
      costPounds = newValue / 0.77
    }
  }
  var costDollars: Float {
    get {
      return costPounds * 0.92
    }
    nonmutating set {}
  }
}

final class Beer {
  var abv: Int {
    get { return 7 }
    set { }
  }
}

class LazyCat {
  lazy var purrs: Int = 10
}
