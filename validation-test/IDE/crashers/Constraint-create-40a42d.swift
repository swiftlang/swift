// {"kind":"complete","original":"20f93e2d","signature":"swift::constraints::Constraint::create(swift::constraints::ConstraintSystem&, swift::constraints::ConstraintKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::ArrayRef<swift::TypeVariableType*>)","signatureAssert":"Assertion failed: ((!typeVariables.empty() || hasError()) && \"Did not find type variables!\"), function getTypeVariables","signatureNext":"ConstraintSystem::matchTypes"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a < b {
  @globalActor struct g {
    static shared =
    func c:
    @g () -> String func d<e, f > (e -> f = #^^#
