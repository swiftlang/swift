// RUN: %target-typecheck-verify-swift -parse-stdlib

import Swift
import Foundation

extension Double {
  init(implicit: CGFloat) {
    print("Double.init(implicit: CGFloat)")
    self.init(implicit)
  }
}
extension CGFloat {
  init(implicit: Double) {
    print("CGFloat.init(implicit: Double)")
    self.init(implicit)
  }
}
public struct III {
  var i: Int
}
extension Int {
  init(implicit: Int16) {
    print("Int.init(implicit: Int16)")
    self.init(implicit)
  }
  init(implicit: Int32) {
    print("Int.init(implicit: Int32)")
    self.init(implicit)
  }
  init(_ implicit: III) {
    print("Int.init(implicit: I)")
    self.init(implicit.i)
  }
}
extension Int32 {
  init(implicit: Int8) {
    print("Int32.init(implicit: Int8)")
    self.init(implicit)
  }
  init(implicit: Int16) {
    print("Int32.init(implicit: Int16)")
    self.init(implicit)
  }
}
extension III {
  public init(_ implicit: Int) {
    print("III.init(implicit: Int)")
    self.init(i: implicit)
  }
}

extension UnsafeRawPointer {
  init(implicit: UnsafeMutableRawPointer) {
    self.init(implicit)
  }
  init(implicit: UnsafeMutablePointer<Pointee>) {
    self.init(implicit)
  }
  init(implicit: UnsafePointer<Pointee>) {
    self.init(implicit)
  }
}
extension UnsafePointer {
  init(implicit: UnsafeMutablePointer<Pointee>) {
    self.init(implicit)
  }
}
extension UnsafePointer where Pointee == Int {
  init(implicit: UnsafeMutablePointer<Int>) {
    self.init(implicit)
  }
}

let x: Double = 99.1
let y: CGFloat = x
let z: Double? = y

let a: Int32 = 997
let b: Int = a
let c: Int8 = 97
let d: Int = c // expected-error {{cannot convert value of type 'Int8' to specified type 'Int'}}
let e = III(i: 888)
let f: Int = e
let h: Int16 = 45
let j: Int32 = h

let optionalMutableRawPointer = UnsafeMutableRawPointer(bitPattern: -3)
let mutableRawPointer = optionalMutableRawPointer!
let immutable: UnsafeRawPointer = mutableRawPointer
//let immutable2: UnsafeRawPointer? = optionalMutableRawPointer
let optionalImmutable: UnsafeRawPointer? = mutableRawPointer
let mutable: UnsafeMutableRawPointer = immutable // expected-error {{cannot convert value of type 'UnsafeRawPointer' to specified type 'UnsafeMutableRawPointer'}}

if mutableRawPointer == immutable || immutable == mutableRawPointer ||
    mutableRawPointer == optionalImmutable || optionalImmutable == mutableRawPointer {
}

func unmutable(immutable: UnsafeRawPointer) -> UnsafeRawPointer {
    return mutableRawPointer
}
func unmutable(optionalImmutable: UnsafeRawPointer?) -> UnsafeRawPointer? {
    return optionalImmutable//optionalMutableRawPointer
}

_ = unmutable(immutable: mutableRawPointer)
_ = unmutable(optionalImmutable: mutableRawPointer)
_ = unmutable(optionalImmutable: optionalMutableRawPointer)

var i = 99
let mutable3 = UnsafeMutablePointer<Int>(&i)
let immutable3 = UnsafePointer<Int>(&i)
let immutable4: UnsafePointer<Int> = mutable3
let immutable5: UnsafePointer<Int>? = mutable3
let immutable6: UnsafePointer<Double> = mutable3 // expected-error {{cannot convert value of type 'UnsafeMutablePointer<Int>' to specified type 'UnsafePointer<Double>'}}
let mutable4: UnsafeMutablePointer<Int> = immutable3 // expected-error {{cannot convert value of type 'UnsafePointer<Int>' to specified type 'UnsafeMutablePointer<Int>'}}
if mutable3 == immutable3 || immutable3 == mutable3 ||
    immutable == immutable3 || immutable == mutable3 {
}

func demutable(immutable: UnsafePointer<Int>) -> UnsafeRawPointer {
    return mutableRawPointer
}
func demutable(optionalImmutable: UnsafePointer<Int>?) -> UnsafeRawPointer? {
    return optionalImmutable//optionalMutableRawPointer
}

_ = demutable(immutable: mutable3)
_ = demutable(optionalImmutable: mutable3)

_ = unmutable(immutable: mutable3)
_ = unmutable(optionalImmutable: mutable3)

var array = [1]
array.withUnsafeMutableBufferPointer {
    _ = demutable(immutable: $0.baseAddress!)
    _ = demutable(optionalImmutable: $0.baseAddress)
}
