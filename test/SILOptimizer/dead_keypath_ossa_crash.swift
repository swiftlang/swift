// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build a resilient module first: the key path's Output type must have opaque
// (resilient) layout to reproduce the issue. (#89791 originally used
// Foundation.Date; a local resilient type keeps this test SDK-independent.)
// RUN: %target-swift-frontend -emit-module %t/ResLib.swift -parse-as-library -O -swift-version 6 -enable-library-evolution -module-name ResLib -emit-module-path %t/ResLib.swiftmodule

// Build the generic key-path wrapper with cross-module optimization so that its
// initializer is inlined into Main, which is what exposes the dead key path.
// RUN: %target-swift-frontend -emit-module %t/BoxLib.swift -parse-as-library -O -swift-version 6 -enable-default-cmo -module-name BoxLib -I %t -emit-module-path %t/BoxLib.swiftmodule

// Optimizing Main used to crash DeadObjectElimination's OSSA ownership verifier
// with "Found a leaked owned value that was never consumed": removing the dead
// key path also removed a struct that consumed an owned value defined outside
// the dead graph, without inserting a compensating destroy_value.
// https://github.com/swiftlang/swift/issues/89791
// RUN: %target-swift-frontend -c %t/Main.swift -parse-as-library -O -swift-version 6 -enable-default-cmo -sil-verify-all -I %t -module-name Main -o /dev/null


//--- ResLib.swift

// A resilient struct, so that `Stamp` has opaque layout in the client.
public struct Stamp {
  public var t: Int
  public init(_ t: Int) { self.t = t }
}

//--- BoxLib.swift

public protocol E<Q> {
  associatedtype Q
}

public protocol M: E where Q == Self {
  associatedtype Fields: L<Self>
  static var fields: Fields { get }
}

public protocol L<Q>: E where Q: M {
  static var all: [any BP] { get }
}

public protocol P<Base, Output>: E where Q == Self, Base: M {
  associatedtype Base
  associatedtype Output
}

public protocol BP<T>: E where Q == T, T: P {
  associatedtype T: P
  var payload: T.Output? { get }
  var keyPath: KeyPath<T.Base, T.Output> { get }
}

public struct Box<T: P>: BP {
  public typealias Q = T

  public let name: String
  public let payload: T.Output?
  public let keyPath: KeyPath<T.Base, T.Output>

  public init(_ name: String, keyPath: KeyPath<T.Base, T.Output>, payload: T.Output? = nil) {
    self.name = name
    self.payload = payload
    self.keyPath = keyPath
  }

  public var all: [any BP] { [self] }
}

//--- Main.swift

import BoxLib
import ResLib

struct Root {
  var value: Int
  var date: Stamp
}

struct IntField: P {
  typealias Base = Root
  typealias Output = Int
}

struct StampField: P {
  typealias Base = Root
  typealias Output = Stamp
}

extension Root: M {
  static var fields: Fields { Fields() }

  struct Fields: L {
    typealias Q = Root

    let value = Box<IntField>("value", keyPath: \.value)
    let unused = Box<IntField>("value", keyPath: \.value)
    let date = Box<StampField>("date", keyPath: \.date)

    static var all: [any BP] {
      Root.fields.value.all + Root.fields.date.all
    }
  }
}
