// XFAIL: *
// FIXME: xfailing the test as there is no longer a free `map` function
// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
[[map)#^A^#
