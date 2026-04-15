// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -c -cxx-interoperability-mode=default -Xcc -std=c++14 -Xcc -fmodules-cache-path=%t
// RUN: %target-swift-frontend %s -c -cxx-interoperability-mode=default -Xcc -std=c++17 -Xcc -fmodules-cache-path=%t
// RUN: %target-swift-frontend %s -c -cxx-interoperability-mode=default -Xcc -std=c++20 -Xcc -fmodules-cache-path=%t

// Ensure that the pre-compiled modules are emitted.
// RUN: stat %t/*/SwiftAndroid-*.pcm
// RUN: stat %t/*/std-*.pcm

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -c -cxx-interoperability-mode=default -Xcc -std=c++17 -Xcc -fmodules-cache-path=%t -DADD_CXXSTDLIB
// RUN: %target-swift-frontend %s -c -cxx-interoperability-mode=default -Xcc -std=c++20 -Xcc -fmodules-cache-path=%t -DADD_CXXSTDLIB

// REQUIRES: OS=linux-android || OS=linux-androideabi

import Android
import Bionic

#if ADD_CXXSTDLIB
import CxxStdlib
#endif

func test() {
#if ADD_CXXSTDLIB
  let _ = std.string()
#endif
}

