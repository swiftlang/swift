// RUN: true

#if _runtime(_ObjC)
import Foundation
#endif

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

public final class Beer {
  public var abv: Int {
    get { return 7 }
    set { }
  }
}

public class LazyCat {
  public lazy var purrs: Int = 10
}

#if _runtime(_ObjC)
public final class FinalCountdown : NSObject {
  public lazy var count: Int = 42
}
#else
public final class FinalCountdown {
  public lazy var count: Int = 42
}
#endif
