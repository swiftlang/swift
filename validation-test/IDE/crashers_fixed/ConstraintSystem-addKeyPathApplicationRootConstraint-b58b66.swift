// {"kind":"complete","original":"238c547b","signature":"swift::constraints::ConstraintSystem::addKeyPathApplicationRootConstraint(swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: ((path.size() == 1 && path[0].getKind() == ConstraintLocator::SubscriptMember) || (path.size() == 2 && path[1].getKind() == ConstraintLocator::KeyPathDynamicMember)), function addKeyPathApplicationRootConstraint","signatureNext":"ConstraintSystem::addKeyPathApplicationConstraint"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a
  var b: [a]
  guard
  case  .c = b[
    #^^#
