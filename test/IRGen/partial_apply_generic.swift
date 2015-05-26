// RUN: %target-swift-frontend %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64

//
// Type parameters
//
infix operator ~> { precedence 255 }

func ~> <Target, Args, Result> (
  target: Target,
  method: Target -> Args -> Result)
  -> Args -> Result
{
  return method(target)
}

protocol Runcible {
  typealias Element
}

struct Mince {}

struct Spoon: Runcible {
  typealias Element = Mince
}

func split<Seq: Runcible>(seq: Seq)(isSeparator: Seq.Element -> Bool) { }

var seq = Spoon()
var x = seq ~> split

//
// Indirect return
//

// CHECK-LABEL: define internal { i8*, %swift.refcounted* } @_TPA__TF21partial_apply_generic5split{{.*}}(%V21partial_apply_generic5Spoon*, %swift.refcounted*)
// CHECK:         [[REABSTRACT:%.*]] = bitcast %V21partial_apply_generic5Spoon* %0 to %swift.opaque*
// CHECK:         tail call { i8*, %swift.refcounted* } @_TF21partial_apply_generic5split{{.*}}(%swift.opaque* [[REABSTRACT]],

struct HugeStruct { var a, b, c, d: Int }
func hugeStructReturn()(h: HugeStruct) -> HugeStruct { return h }
var y = hugeStructReturn()
// CHECK-LABEL: define internal void @_TPA__TF21partial_apply_generic16hugeStructReturn{{.*}}(%V21partial_apply_generic10HugeStruct* noalias sret, %V21partial_apply_generic10HugeStruct*, %swift.refcounted*) {{.*}} {
// CHECK:   tail call void @_TF21partial_apply_generic16hugeStructReturn{{.*}}(%V21partial_apply_generic10HugeStruct* noalias sret %0, %V21partial_apply_generic10HugeStruct* %1)
// CHECK:   ret void
// CHECK: }

//
// Witness method
//
protocol Protein {
  static func veganOrNothing() -> Protein?
  static func paleoDiet() throws -> Protein
}

enum CarbOverdose : ErrorType {
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

func healthyLunch<T: Protein>(t: T) -> () -> Protein? {
  return T.veganOrNothing
}

let f = healthyLunch(Chicken())

func dietaryFad<T: Protein>(t: T) -> () throws -> Protein {
  return T.paleoDiet
}

let g = dietaryFad(Chicken())
do {
  try g()
} catch {}
