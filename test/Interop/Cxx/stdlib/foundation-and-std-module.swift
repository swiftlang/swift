// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -c -enable-experimental-cxx-interop -Xcc -std=c++17
// RUN: %target-swift-frontend %s -c -enable-experimental-cxx-interop -Xcc -std=c++20

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -c -enable-experimental-cxx-interop -Xcc -std=c++17 -module-cache-path %t -DADD_CXXSTDLIB
// RUN: %target-swift-frontend %s -c -enable-experimental-cxx-interop -Xcc -std=c++20 -DADD_CXXSTDLIB

// RUN: ls -R %/t | %FileCheck %s

// REQUIRES: no_asan

#if canImport(Foundation)
import Foundation
#endif

#if ADD_CXXSTDLIB
import CxxStdlib
#endif

func test() {
#if ADD_CXXSTDLIB
  let _ = std.string()
#endif
}

// CHECK: std-{{.*}}.pcm
