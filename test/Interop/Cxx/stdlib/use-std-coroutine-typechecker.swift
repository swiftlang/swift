// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++20 -Xcc -fcoroutines
// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++23 -Xcc -fcoroutines

// Verify that C++ headers including <coroutine> can be imported into Swift,
// even though C++ coroutines themselves are not usable from Swift.

// REQUIRES: std_coroutine

import StdCoroutine

let _ = isSuspendNeverReady()
