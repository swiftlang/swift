//===----------------------------------------------------------------------===//
// Slice<T> Type
//===----------------------------------------------------------------------===//

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

   subscript (i : Int) -> T {
     get {
       if i >= length {
         Builtin.trap()
       }

       return (base + i).get()
     }

     set {
       if i >= length {
         Builtin.trap()
       }

       (base + i).set(value)
     }
   }

   // Slicing via subscripting with a range.
   subscript (rng : IntRange) -> Slice<T> {
     get {
       if !(rng.min <= length && rng.max <= length) {
         Builtin.trap()
       }
       typealias SliceT = Slice<T>
       return SliceT(base + rng.min, rng.max - rng.min, owner)
     }

     set {
       if !(value.length == rng.max - rng.min) {
         Builtin.trap()
       }

       // Common case: the elements were updated in place, so we do not have to
       // perform any updates.
       var destStart = base + rng.min
       if value.base == destStart {
         return
       }

       // If the start of the destination slice falls inside the source slice,
       // copy backwards.
       if destStart >= value.base && destStart < value.base + value.length {
         var destEnd = destStart + value.length
         for i in value {
           --destEnd
           destEnd.set(i)
         }

         return
       }

       // Copy the data.
       for i in value {
         destStart.set(i)
         ++destStart
       }
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

  func map(f : (T) -> T) -> T[] {
    var r = new T[length]
    for i in 0 .. length { r[i] = f(this[i]) }
    return r
  }

  func copy() -> T[] {
    var result = new T[length]
    for i in 0..length { result[i] = this[i] }
    return result
  }

  typealias Element = T

  func isEmpty() ->Bool { return length == 0 }
  func getFirstAndAdvance() -> T {
    var prev = base
    base = base + 1
    length = length - 1
    return prev.get()
  }
}
