// REQUIRES: VENDOR=apple

// Required DummyFramework imported is only built for those two CPU architectures.
// REQUIRES: CPU=arm64 || CPU=x86_64
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t/module-cache)
// RUN: not %target-swift-frontend-typecheck -disable-implicit-concurrency-module-import -target %target-cpu-apple-macosx13.0 -F %S/Inputs %s -module-cache-path %t/module-cache
// RUN: %target-swift-frontend-typecheck -disable-implicit-concurrency-module-import -target %target-cpu-apple-macosx13.0 -F %S/Inputs %s -module-cache-path %t/module-cache -backup-module-interface-path %S/Inputs/alternative-interfaces

import DummyFramework
