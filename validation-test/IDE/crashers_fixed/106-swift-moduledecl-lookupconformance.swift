// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
protocol c{#^A^#class B<d,g:T>:c
class T
