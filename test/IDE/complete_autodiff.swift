// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE | %FileCheck %s

protocol DifferentiableRequirements {
  @differentiable
  func f(_ x: Float) -> Float
}

struct Foo : DifferentiableRequirements {
  @differentiable
  func f#^COMPLETE^#
}

// CHECK-LABEL: Begin completions
// CHECK: func f(_ x: Float) -> Float
// CHECK: End completions
