// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

// https://github.com/apple/swift/issues/45942

protocol A{class T#^A^#class b:Array<T>
