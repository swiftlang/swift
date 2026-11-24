// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::matchTypes(swift::Type, swift::Type, swift::constraints::ConstraintKind, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"matchDeepTypeArguments"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b, c> {
  static var d: a<c, b>
  let <#pattern#>: a<_, _> = .#^^#
}
