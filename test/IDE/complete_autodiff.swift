// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE1 | %FileCheck --check-prefix=COMPLETE1 %s

protocol DifferentiableRequirements {
  @differentiable
  func f(_ x: Float) -> Float
}

struct Complete1 : DifferentiableRequirements {
  @differentiable
  func f#^COMPLETE1^#
}

// COMPLETE1-LABEL: Begin completions
// COMPLETE1: func f(_ x: Float) -> Float
// COMPLETE1: End completions

struct Complete2 : DifferentiableRequirements {
  @differentiable
  func f(_ x: Float)#^COMPLETE2^#
}

// COMPLETE2-LABEL: Begin completions
// COMPLETE2: func f(_ x: Float) -> Float
// COMPLETE2: End completions
