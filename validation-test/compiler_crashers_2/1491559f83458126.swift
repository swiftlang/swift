// {"signature":"swift::constraints::ConstraintGraph::removeConstraint(swift::TypeVariableType*, swift::constraints::Constraint*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
[{
    a
    b
  }
c.d<b>
