// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::addKeyPathApplicationRootConstraint(swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: ((path.size() == 1 && path[0].getKind() == ConstraintLocator::SubscriptMember) || (path.size() == 2 && path[1].getKind() == ConstraintLocator::KeyPathDynamicMember)), function addKeyPathApplicationRootConstraint"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a {
  @dynamicMemberLookup enum b {
    subscript <c>(dynamicMember d: KeyPath<a, c>) -> c {
      @dynamicMemberLookup class e {c { self[
#^COMPLETE^#}
        subscript(dynamicMember d: KeyPath<b, c>) -> c
      }
    }
  }
}
