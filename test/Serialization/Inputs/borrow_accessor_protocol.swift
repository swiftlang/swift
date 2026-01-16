public protocol P {
  var id: NonTrivial { borrow mutate }
}

public class Klass {
  public init() {}
}

public struct NonTrivial {
  public var k: Klass
}

// Protocol requirements witnessed via borrow/mutate accessors
public struct S1 : P {
  var _id: NonTrivial

  public var id: NonTrivial {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

// Protocol requirements witnessed via stored property
public struct S2 : P {
  public var id: NonTrivial
}

@frozen
public struct S3 : P {
  var _id: NonTrivial

  public var id: NonTrivial {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

@frozen
public struct S4 : P {
  public var id: NonTrivial
}

public protocol Q {
  var id: NonTrivial { get set }
}

public struct S5 : P {
  var _id: NonTrivial

  public var id: NonTrivial {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

@frozen
public struct S6 : P {
  var _id: NonTrivial

  public var id: NonTrivial {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

public struct NC : ~Copyable {
  public init() {}
}

public protocol NCP : ~Copyable {
  var id: NC {borrow mutate}
}

public struct NCWrapper1 : NCP & ~Copyable {
  var _id: NC

  public var id: NC {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

public struct NCWrapper2: NCP & ~Copyable {
  public var id: NC
}

@frozen
public struct NCWrapper3 : NCP & ~Copyable {
  var _id: NC

  public var id: NC {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

@frozen
public struct NCWrapper4: NCP & ~Copyable {
  public var id: NC
}
