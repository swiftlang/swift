// REQUIRES: VENDOR=apple

// Required DummyFramework imported is only built for macOS supported archs (x86_64, arm64, arm64e)
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t/module-cache)
// RUN: not %target-swift-frontend-typecheck -disable-implicit-concurrency-module-import -target %target-cpu-apple-macosx13.0 -F %S/Inputs %s -module-cache-path %t/module-cache
// RUN: %target-swift-frontend-typecheck -disable-implicit-concurrency-module-import -target %target-cpu-apple-macosx13.0 -F %S/Inputs %s -module-cache-path %t/module-cache -backup-module-interface-path %S/Inputs/alternative-interfaces

import DummyFramework
