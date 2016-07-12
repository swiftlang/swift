// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
typealias f=B{func a{f#^A^#}}struct B{let d
