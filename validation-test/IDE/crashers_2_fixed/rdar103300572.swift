// RUN: %swift-ide-test -code-completion -code-completion-token COMPLETE -code-completion-annotate-results -source-filename %s

protocol P { }
func f(_ p: some P) {
  #^COMPLETE^#
}
