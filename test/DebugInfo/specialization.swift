// RUN: %target-swift-frontend -O %s -disable-llvm-optzns -emit-sil -g -o - | FileCheck %s

// CHECK: sil shared [noinline] @_TTSg5SiSis21IntegerArithmeticTypes__
// CHECK-SAME: _TF14specialization3sumuRxs21IntegerArithmeticTyperFTxx_x
// CHECK-SAME: $@convention(thin) (@out Int, @in Int, @in Int) -> () {
// CHECK: bb0(%0 : $*Int, %1 : $*Int, %2 : $*Int):
// CHECK:  debug_value_addr %1 : $*Int, let, name "i", argno 1
// CHECK:  debug_value_addr %2 : $*Int, let, name "j", argno 2
  
@inline(never)
public func sum<T : IntegerArithmeticType>(i : T, _ j : T) -> T {
  let result = i + j
  return result
}

public func inc(inout i : Int) {
  i = sum(i, 1)
}
