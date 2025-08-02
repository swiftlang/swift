// {"kind":"typecheck","signature":"swift::constraints::ConstraintGraphNode::addConstraint(swift::constraints::Constraint*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
[{
    a
    b
  }
c.d<b>
