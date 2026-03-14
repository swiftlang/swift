// {"kind":"complete","original":"dbc80c20","signature":"swift::AbstractStorageDecl::setSynthesizedAccessor(swift::AccessorKind, swift::AccessorDecl*)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype c: a
  var b: c
}
protocol d: a where c == Never
  extension Never: d
    extension d {  b: Never = #^^#
