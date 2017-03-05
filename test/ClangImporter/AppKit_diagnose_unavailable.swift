// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s 2>&1 | %FileCheck -check-prefix=CHECK %s
//
// REQUIRES: OS=macosx

import AppKit

// rdar://30480908 notes the presence of spurious deprecation warnings like
// '<unknown>:0: warning: 'cacheParamsComputed' is deprecated'.  Check that
// doesn't occur here.
func rdar30480908() {
  // Catch-all for SourceLoc-less diagnostics
  // CHECK-NOT: <unknown>:0:
  
  // CHECK-NOT: <unknown>:0: warning: 'cacheParamsComputed' is deprecated
  // CHECK-NOT: <unknown>:0: warning: 'cacheAlphaComputed' is deprecated
  // CHECK-NOT: <unknown>:0: warning: 'keepCacheWindow' is deprecated
  // CHECK-NOT: <unknown>:0: error: 'memoryless' is unavailable
  // CHECK-NOT: Metal.MTLCommandBufferError:55:14: note: 'memoryless' has been explicitly marked unavailable here
  // CHECK-NOT:     case memoryless
  let vc = NSStig() 
}

