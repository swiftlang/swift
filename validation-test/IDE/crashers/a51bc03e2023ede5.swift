// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::buildDisjunctionForOptionalVsUnderlying(swift::Type, swift::Type, swift::constraints::ConstraintLocator*)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
class a {b: c! = b #^COMPLETE^#
