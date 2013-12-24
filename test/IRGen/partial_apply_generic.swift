// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// FIXME: This should be a SIL test, but we can't parse generic types yet.

operator infix ~> { precedence 255 }

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

// CHECK-LABEL: define internal { i8*, %swift.refcounted* } @_TPA__TF21partial_apply_generic5splitUS_8Runcible__FT3seqQ__FT11isSeparatorFQQ_7ElementSb_T_(%V21partial_apply_generic5Spoon* noalias, %swift.refcounted*)
// CHECK:         [[REABSTRACT:%.*]] = bitcast %V21partial_apply_generic5Spoon* %0 to %swift.opaque*
// CHECK:         tail call { i8*, %swift.refcounted* } @_TF21partial_apply_generic5splitUS_8Runcible__FT3seqQ__FT11isSeparatorFQQ_7ElementSb_T_(%swift.opaque* noalias [[REABSTRACT]],
