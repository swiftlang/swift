// RUN:  %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
{protocol a{typealias B:a
var T>typealias d:a{#^A^#
