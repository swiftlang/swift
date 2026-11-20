// {"kind":"complete","original":"f32c071c","signature":"swift::LifetimeDependentInterface::canConvertTargetTo(swift::LifetimeDependenceInfo const&, swift::LifetimeDependentInterface const&) const","signatureNext":"LifetimeDependentInterface::canConvertTo"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
}
  ?? Span < Double > #^^#
