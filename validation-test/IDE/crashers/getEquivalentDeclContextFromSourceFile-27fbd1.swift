// {"kind":"complete","signature":"(anonymous namespace)::getEquivalentDeclContextFromSourceFile(swift::DeclContext*, swift::SourceFile*)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
do { func a { #^^# #^b^#
