// {"kind":"typecheck","signature":"swift::DifferentiableAttr::hasBeenTypeChecked() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@differentiable () let a, b
