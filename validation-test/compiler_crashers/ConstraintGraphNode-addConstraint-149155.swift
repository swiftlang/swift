// {"kind":"typecheck","signature":"swift::constraints::ConstraintGraphNode::addConstraint(swift::constraints::Constraint*)","signatureAssert":"Assertion failed: (ConstraintIndex.count(constraint) == 0 && \"Constraint re-insertion\"), function addConstraint","signatureNext":"ConstraintGraph::addConstraint"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
[{
    a
    b
  }
c.d<b>
