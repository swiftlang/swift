// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

public typealias CustomGetter<T> = (Int) -> T #^COMPLETE^#
