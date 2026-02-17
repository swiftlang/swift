// RUN: %target-swift-frontend -enable-experimental-feature BorrowAndMutateAccessors -emit-silgen %s -verify
// RUN: %target-swift-frontend -enable-experimental-feature BorrowAndMutateAccessors -emit-silgen -enable-library-evolution %s -verify

public protocol P {
  associatedtype Element
  var id: Element { borrow mutate }
}

public struct S1 : P {
  public typealias Element = Int
  var _id: Int

  public var id: Int {
    borrow { // expected-error{{cannot witness borrow requirement for result type 'Int'}}
      return _id
    }
    mutate {
      return &_id
    }
  }
}

public struct S2 : P {
  public typealias Element = Int
  public var id: Int // expected-error{{cannot witness borrow requirement for result type 'Int'}}
}

public struct S3 : P {
  public typealias Element = String
  var _id: String

  public var id: String {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

public protocol ThingHolder {
  associatedtype Thing
  var thing: Thing { borrow }
}

public struct ClosureHolder: ThingHolder {
  var theThing: () -> ()
  public var thing: () -> () { borrow { return theThing } } // expected-error{{cannot witness borrow requirement for result type '() -> ()'}}
}

public protocol Q {
  associatedtype Element
  var id: Element { borrow mutate }
}

public struct OptionalConformer<Element> : Q {
  var _id: Element?

  public var id: Element? {
    borrow {
      return _id
    }
    mutate {
      return &_id
    }
  }
}

