// RUN: %target-swift-frontend %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64

// Broken due to lack of NonFixedOffsets and NecessaryBindings on HeapLayouts
//   -JoeG
// XFAIL: *

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

// CHECK-LABEL: define internal { i8*, %swift.refcounted* } @_TPA__TF21partial_apply_generic5split{{.*}}(%V21partial_apply_generic5Spoon* noalias, %swift.refcounted*)
// CHECK:         [[REABSTRACT:%.*]] = bitcast %V21partial_apply_generic5Spoon* %0 to %swift.opaque*
// CHECK:         tail call { i8*, %swift.refcounted* } @_TF21partial_apply_generic5split{{.*}}(%swift.opaque* noalias [[REABSTRACT]],

struct HugeStruct { var a, b, c, d: Int }
func hugeStructReturn()(h: HugeStruct) -> HugeStruct { return h }
var y = hugeStructReturn()
// CHECK-LABEL: define internal void @_TPA__TF21partial_apply_generic16hugeStructReturn{{.*}}(%V21partial_apply_generic10HugeStruct* noalias sret, i64, i64, i64, i64) {
// CHECK:   tail call void @_TF21partial_apply_generic16hugeStructReturn{{.*}}(%V21partial_apply_generic10HugeStruct* noalias sret %0, i64 %1, i64 %2, i64 %3, i64 %4)
// CHECK:   ret void
// CHECK: }
