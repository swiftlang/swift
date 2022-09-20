// RUN: %target-swift-emit-silgen %s | %FileCheck %s

struct Tree<T> {
  struct Branch<B> {
    struct Nest<N> {
      struct Egg {}
    }
  }
}

// CHECK: extension Tree.Branch.Nest.Egg {
// CHECK:   static func tweet()
// CHECK: }

// CHECK: extension Tree.Branch.Nest.Egg where T == Int {
// CHECK:   static func twoot()
// CHECK: }

// CHECK: extension Tree.Branch.Nest.Egg where T == Int, B == String {
// CHECK:   static func twote()
// CHECK: }

// CHECK: extension Tree.Branch.Nest.Egg where T == Int, B == String, N == () {
// CHECK:   static func twite()
// CHECK: }

// CHECK: @$s31mangling_specialized_extensions4TreeV6BranchV4NestV3EggV5tweetyyFZ : $@convention(method) <T><B><N> (@thin Tree<T>.Branch<B>.Nest<N>.Egg.Type) -> ()
extension Tree.Branch.Nest.Egg { static func tweet() {} }

// CHECK: @$s31mangling_specialized_extensions4TreeV6BranchV4NestV3EggVAASiRszrlE5twootyyFZ : $@convention(method) <T where T == Int><B><N> (@thin Tree<Int>.Branch<B>.Nest<N>.Egg.Type) -> ()
extension Tree<Int>.Branch.Nest.Egg { static func twoot() {} }

// CHECK: @$s31mangling_specialized_extensions4TreeV6BranchV4NestV3EggVAASiRszSSRsd__rlE5twoteyyFZ : $@convention(method) <T where T == Int><B where B == String><N> (@thin Tree<Int>.Branch<String>.Nest<N>.Egg.Type) -> ()
extension Tree<Int>.Branch<String>.Nest.Egg { static func twote() {} }

// CHECK: @$s31mangling_specialized_extensions4TreeV6BranchV4NestV3EggVAASiRszSSRsd__ytRsd0__rlE5twiteyyFZ : $@convention(method) (@thin Tree<Int>.Branch<String>.Nest<()>.Egg.Type) -> ()
extension Tree<Int>.Branch<String>.Nest<Void>.Egg { static func twite() {} }
