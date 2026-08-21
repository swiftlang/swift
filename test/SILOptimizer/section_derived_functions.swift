// RUN: %target-swift-frontend -emit-sil -O -parse-as-library %s | %FileCheck %s

// The functions that the optimizer derives from a function with a '@section'
// go into the same section.

public final class Box {
  var x = 0
}

@inline(never)
@section("__TEXT,boot")
func deadArgument(_ b: Box, _ unused: Box) -> Int { b.x }

@inline(never)
@section("__TEXT,boot")
func takesClosure(_ fn: () -> Int) -> Int { fn() + fn() }

@inline(never)
@section("__TEXT,boot")
func promotesCapture(_ b: Box) -> Int {
  var x = 1
  @inline(never)
  func inner() -> Int { x + b.x }
  x = 2
  return inner()
}

@inline(never)
@section("__TEXT,boot")
func generic<T: BinaryInteger>(_ value: T) -> T { value + 1 }

public func driver(_ b: Box) -> Int {
  var total = deadArgument(b, b)
  total += takesClosure { b.x }
  total += promotesCapture(b)
  total += generic(b.x)
  return total
}

// The generic specialization.
// CHECK-DAG: sil {{.*}}[section "__TEXT,boot"] @$s25section_derived_functions7genericyxxSzRzlFSi_Tg5

// The function-signature-optimized function that takes over the body of
// 'deadArgument', as well as the thunk that is left behind.
// CHECK-DAG: sil {{.*}}[signature_optimized_thunk] {{.*}}[section "__TEXT,boot"] @$s25section_derived_functions12deadArgumentySiAA3BoxC_ADtF
// CHECK-DAG: sil {{.*}}[section "__TEXT,boot"] @$s25section_derived_functions12deadArgumentySiAA3BoxC_ADtFTf4nd_n

// The closure-specialized version of 'takesClosure'.
// CHECK-DAG: sil {{.*}}[section "__TEXT,boot"] @$s25section_derived_functions12takesClosureyS2iyXEF{{.*}}Tf1c_n

// The capture-promoted version of 'inner'.
// CHECK-DAG: sil {{.*}}[section "__TEXT,boot"] @$s25section_derived_functions15promotesCaptureySiAA3BoxCF5innerL_SiyFTf0sn_n
