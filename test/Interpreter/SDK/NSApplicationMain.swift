// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: %target-build-swift -parse %s
// REQUIRES: OS=macosx

import AppKit
NSApplicationMain(Process.argc, Process.unsafeArgv)
