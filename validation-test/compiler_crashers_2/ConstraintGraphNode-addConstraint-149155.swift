// {"kind":"typecheck","signature":"swift::constraints::ConstraintGraphNode::addConstraint(swift::constraints::Constraint*)","signatureAssert":"Assertion failed: (ConstraintIndex.count(constraint) == 0 && \"Constraint re-insertion\"), function addConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
[{
    a
    b
  }
c.d<b>
