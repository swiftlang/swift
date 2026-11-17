// REQUIRES: OS=macosx

// RUN: %target-swift-frontend -load-pass-plugin=nonexistent.dylib %s -emit-ir -o /dev/null 2>&1 | %FileCheck -check-prefix=CHECK-UNABLE-LOAD %s
// CHECK-UNABLE-LOAD: error: unable to load plugin 'nonexistent.dylib': 'Could not load library{{.*}}'

// swift-frontend is linked with -no_exported_symbols on Darwin, so a pass
// plugin that expects to resolve LLVM symbols from the executable cannot be
// loaded. Only the diagnostic for a plugin that fails to load is tested here.

func empty() {}
