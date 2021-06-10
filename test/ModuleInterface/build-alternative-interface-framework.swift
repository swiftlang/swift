// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t/module-cache)
// RUN: not %target-swift-frontend-typecheck -disable-implicit-concurrency-module-import -target arm64-apple-macosx13.0 -F %S/Inputs %s -module-cache-path %t/module-cache
// RUN: %target-swift-frontend-typecheck -disable-implicit-concurrency-module-import -target arm64-apple-macosx13.0 -F %S/Inputs %s -module-cache-path %t/module-cache -backup-module-interface-path %S/Inputs/alternative-interfaces

import DummyFramework
