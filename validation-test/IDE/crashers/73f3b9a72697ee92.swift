// {"kind":"complete","signature":"swift::GenericSignatureImpl::isValidTypeParameter(swift::Type) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype c: a
  var b: c
}
protocol d: a where c == Never
  extension Never: d
    extension d {
      #^COMPLETE^# var
      b: Never
