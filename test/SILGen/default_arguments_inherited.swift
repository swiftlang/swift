// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @_T027default_arguments_inherited4doItyyF : $@convention(thin) () -> () {
func doIt() {
  // CHECK: [[ARG1:%.*]] = function_ref @_T027default_arguments_inherited5PuppyCACyxq_GxSg1t_q_Sg1utcfcfA_
  // CHECK: apply [[ARG1]]<Int, String>({{.*}})
  // CHECK: [[ARG2:%.*]] = function_ref @_T027default_arguments_inherited5PuppyCACyxq_GxSg1t_q_Sg1utcfcfA0_
  // CHECK: apply [[ARG2]]<Int, String>({{.*}})
  _ = Chipmunk()

  // CHECK: [[ARG1:%.*]] = function_ref @_T027default_arguments_inherited5PuppyCACyxq_GxSg1t_q_Sg1utcfcfA_
  // CHECK: apply [[ARG1]]<Int, String>(%{{.*}})
  // CHECK: [[ARG2:%.*]] = function_ref @_T027default_arguments_inherited5PuppyCACyxq_GxSg1t_q_Sg1utcfcfA0_
  // CHECK: apply [[ARG2]]<Int, String>(%{{.*}})
  _ = Kitten<String>()

  // CHECK: [[ARG1:%.*]] = function_ref @_T027default_arguments_inherited5PuppyCACyxq_GxSg1t_q_Sg1utcfcfA_
  // CHECK: apply [[ARG1]]<String, Int>(%{{.*}})
  // CHECK: [[ARG2:%.*]] = function_ref @_T027default_arguments_inherited5PuppyCACyxq_GxSg1t_q_Sg1utcfcfA0_
  // CHECK: apply [[ARG2]]<String, Int>(%{{.*}})
  _ = Goldfish<String>.Shark<Int>()
}
