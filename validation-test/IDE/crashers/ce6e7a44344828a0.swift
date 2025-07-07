// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::addUnresolvedValueMemberConstraint(swift::Type, swift::DeclNameRef, swift::Type, swift::DeclContext*, swift::FunctionRefInfo, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
enum a }
{
var b: a func c { switch b {
#^COMPLETE^#
