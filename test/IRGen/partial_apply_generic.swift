// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

//
// Type parameters
//
infix operator ~> { precedence 255 }

func ~> <Target, Args, Result> (
  target: Target,
  method: (Target) -> (Args) -> Result)
  -> (Args) -> Result
{
  return method(target)
}

protocol Runcible {
  associatedtype Element
}

struct Mince {}

struct Spoon: Runcible {
  typealias Element = Mince
}

func split<Seq: Runcible>(_ seq: Seq) -> ((Seq.Element) -> Bool) -> () {
  return {(isSeparator: (Seq.Element) -> Bool) in
    return ()
  }
}
var seq = Spoon()
var x = seq ~> split

//
// Indirect return
//

// CHECK-LABEL: define internal swiftcc { i8*, %swift.refcounted* } @_T021partial_apply_generic5split{{[_0-9a-zA-Z]*}}FTA(%T21partial_apply_generic5SpoonV* noalias nocapture, %swift.refcounted* swiftself)
// CHECK:         [[REABSTRACT:%.*]] = bitcast %T21partial_apply_generic5SpoonV* %0 to %swift.opaque*
// CHECK:         tail call swiftcc { i8*, %swift.refcounted* } @_T021partial_apply_generic5split{{[_0-9a-zA-Z]*}}F(%swift.opaque* noalias nocapture [[REABSTRACT]],

struct HugeStruct { var a, b, c, d: Int }
struct S {
  func hugeStructReturn(_ h: HugeStruct) -> HugeStruct { return h }
}

let s = S()
var y = s.hugeStructReturn
// CHECK-LABEL: define internal swiftcc { i64, i64, i64, i64 } @_T021partial_apply_generic1SV16hugeStructReturnAA04HugeE0VAFFTA(i64, i64, i64, i64, %swift.refcounted* swiftself) #0 {
// CHECK: entry:
// CHECK:   %5 = tail call swiftcc { i64, i64, i64, i64 } @_T021partial_apply_generic1SV16hugeStructReturnAA04HugeE0VAFF(i64 %0, i64 %1, i64 %2, i64 %3)
// CHECK:   ret { i64, i64, i64, i64 } %5
// CHECK: }

//
// Witness method
//
protocol Protein {
  static func veganOrNothing() -> Protein?
  static func paleoDiet() throws -> Protein
}

enum CarbOverdose : Error {
  case Mild
  case Severe
}

class Chicken : Protein {
  static func veganOrNothing() -> Protein? {
    return nil
  }

  static func paleoDiet() throws -> Protein {
    throw CarbOverdose.Severe
  }
}

func healthyLunch<T: Protein>(_ t: T) -> () -> Protein? {
  return T.veganOrNothing
}

let f = healthyLunch(Chicken())

func dietaryFad<T: Protein>(_ t: T) -> () throws -> Protein {
  return T.paleoDiet
}

let g = dietaryFad(Chicken())
do {
  try g()
} catch {}
