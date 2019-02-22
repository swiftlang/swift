// https://bugs.swift.org/browse/TF-198: `@dynamicCallable` REPL completer crash.
// RUN: %target-swift-ide-test -repl-code-completion -source-filename=%s

// TODO(TF-214): Require `python` lit feature, after it is created.

import Python
Python.str("name").strip(
