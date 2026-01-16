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

