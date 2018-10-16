// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

// When we synthesize an inherited designated initializer, the default
// arguments are still conceptually rooted on the base declaration.
// When we call such an initializer, we have to construct substitutions
// in terms of the base class generic signature, rather than the
// derived class generic signature.

class Puppy<T, U> {
  init(t: T? = nil, u: U? = nil) {}
}

class Chipmunk : Puppy<Int, String> {}

class Kitten<V> : Puppy<Int, V> {}

class Goldfish<T> {
  class Shark<U> : Puppy<T, U> {}
}

// CHECK-LABEL: sil hidden @$s27default_arguments_inherited4doItyyF : $@convention(thin) () -> () {
func doIt() {
  // CHECK: [[ARG1:%.*]] = function_ref @$s27default_arguments_inherited5PuppyC1t1uACyxq_GxSg_q_SgtcfcfA_
  // CHECK: apply [[ARG1]]<Int, String>({{.*}})
  // CHECK: [[ARG2:%.*]] = function_ref @$s27default_arguments_inherited5PuppyC1t1uACyxq_GxSg_q_SgtcfcfA0_
  // CHECK: apply [[ARG2]]<Int, String>({{.*}})
  _ = Chipmunk()

  // CHECK: [[ARG1:%.*]] = function_ref @$s27default_arguments_inherited5PuppyC1t1uACyxq_GxSg_q_SgtcfcfA_
  // CHECK: apply [[ARG1]]<Int, String>(%{{.*}})
  // CHECK: [[ARG2:%.*]] = function_ref @$s27default_arguments_inherited5PuppyC1t1uACyxq_GxSg_q_SgtcfcfA0_
  // CHECK: apply [[ARG2]]<Int, String>(%{{.*}})
  _ = Kitten<String>()

  // CHECK: [[ARG1:%.*]] = function_ref @$s27default_arguments_inherited5PuppyC1t1uACyxq_GxSg_q_SgtcfcfA_
  // CHECK: apply [[ARG1]]<String, Int>(%{{.*}})
  // CHECK: [[ARG2:%.*]] = function_ref @$s27default_arguments_inherited5PuppyC1t1uACyxq_GxSg_q_SgtcfcfA0_
  // CHECK: apply [[ARG2]]<String, Int>(%{{.*}})
  _ = Goldfish<String>.Shark<Int>()
}
