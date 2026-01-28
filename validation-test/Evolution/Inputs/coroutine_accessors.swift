#if BEFORE // _read/_modify

public struct ResilientStruct {
  var _s : Int = 0
  public init() {}

  public var s : Int {
    _read {
      yield _s
    }

    _modify {
      yield &_s
    }
  }
}

// A `set` implies an implicit guaranteed `_modify` accessor.
public struct ResilientStructWithImplicitModify {
  var _s : Int = 0

  public init() {}

  public var s : Int {
    _read {
      yield _s
    }

    set {
      _s = newValue
    }
  }
}

public enum ResilientEnum {
  case a(Int)
  case b(Int)

  public var s : Int {
    _read {
      switch self {
        case .a(let i):
          yield i
        case .b(let i):
          yield i
      }
    }

    _modify {
      switch self {
        case .a(let i):
          var tmp = i
          yield &tmp
          self = .a(tmp)
        case .b(let i):
          var tmp = i
          yield &tmp
          self = .b(tmp)
      }
    }
  }
}

public class ResilientClass {
  var _s : Int = 0

  public init() {}

  public var s : Int {
    _read {
      yield _s
    }

    _modify {
      yield &_s
    }
  }
}

#else // yielding borrow/mutate

public struct ResilientStruct {
  var _s : Int = 0

  public init() {}

  public var s : Int {
    yielding borrow {
      yield _s
    }

    yielding mutate {
      yield &_s
    }
  }
}

// A `set` implies an implicit guaranteed `_modify` accessor.
public struct ResilientStructWithImplicitModify {
  var _s : Int = 0

  public init() {}

  public var s : Int {
    yielding borrow {
      yield _s
    }

    set {
      _s = newValue
    }
  }
}

public enum ResilientEnum {
  case a(Int)
  case b(Int)

  public var s : Int {
    yielding borrow {
      switch self {
        case .a(let i):
          yield i
        case .b(let i):
          yield i
      }
    }

    yielding mutate {
      switch self {
        case .a(let i):
          var tmp = i
          yield &tmp
          self = .a(tmp)
        case .b(let i):
          var tmp = i
          yield &tmp
          self = .b(tmp)
      }
    }
  }
}

public class ResilientClass {
  var _s : Int = 0

  public init() {}

  public var s : Int {
    yielding borrow {
      yield _s
    }

    yielding mutate {
      yield &_s
    }
  }
}
#endif
