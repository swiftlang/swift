// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -cxx-interoperability-mode=default -module-cache-path %t/mc -Xcc -Wpedantic -Xcc -Werror

// Warning flags that the client passes for its own code should not cause the
// toolchain's C++ shim modules to fail to build.

import CxxStdlib

let s = std.string("a")
