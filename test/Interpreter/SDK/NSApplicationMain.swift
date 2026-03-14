// RUN: %empty-directory(%t/cache)
// RUN: %target-build-swift -typecheck %s -module-cache-path %t/cache
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import AppKit
NSApplicationMain(CommandLine.argc, CommandLine.unsafeArgv)
