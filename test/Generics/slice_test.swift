// RUN: %swift %s -parse -parse-as-library -verify

import Builtin
import swift

// Test a WIP implementation of Slice<T>

struct UnsafePointer<T> {
  var value : Builtin.RawPointer

  func get() -> T {
    return Builtin.load(value)
  }

  func set(newvalue : T) {
    Builtin.assign(newvalue, value)
  }
}

struct Slice<T> : Range, Enumerable {
   var base : UnsafePointer<T>
   var length : Int
   var owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> Slice<T> {
     typealias UnsafePtr = UnsafePointer<T>
     typealias SliceT = Slice<T>
     return SliceT(UnsafePtr(base), Int(length) & Int64.max(), owner)
   }

   subscript (i : Int) -> String {
     get {
       if i >= length {
         Builtin.trap()
       }

       var ptr = base + i
       return Builtin.load(ptr.value)
     }

     set {
       if i >= length {
         Builtin.trap()
       }

       var ptr = base + i
       Builtin.assign(value, ptr.value)
     }
   }

   typealias Elements = Slice<T>
   func getElements() -> Slice<T> { return this }
   
   // FIXME: replPrint doesn't work because T doesn't conform to an
   // appropriate protocol, and we have no way to check it dynamically.
/*
   func replPrint() {
     print('[')
     var first = true
     var total = 0
     for i in this {
       if first {
         first = false
       } else {
         print(", ")
       }
       i.replPrint()
       total = total + 1
       if total > 50 {
         print(" ...]")
         return
       }
     }
     print(']')
   }
*/
  func each(f : (T) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : T, f : (T, T) -> T) -> T {
    for i in this { val = f(val, i) }
    return val
  }

  // FIXME: Map requires GenericSubscriptExpr, and that "new T[length]"
  // returns a Slice<T>.
/*
  func map(f : (T) -> T) -> Slice<T> {
    var r = new T[length]
    for i in 0 .. length { r[i] = f(this[i]) }
    return r
  }
*/

  typealias Element = T

  func isEmpty() ->Bool { return length == 0 }
  func getFirstAndAdvance() -> T {
    var prev = base
    base = base + 1
    length = length - 1
    return Builtin.load(prev.value)
  }
}

func [infix_left=190] + <T> (lhs : UnsafePointer<T>,
                         rhs : Int64) -> UnsafePointer<T> {
  typealias UnsafePtr = UnsafePointer<T>
  return UnsafePtr(Builtin.gep_Int64(lhs.value, (rhs * Int(Builtin.sizeof(T))).value))
}
