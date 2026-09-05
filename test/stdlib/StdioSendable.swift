//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-swift-frontend -swift-version 6 -emit-sil -o /dev/null -verify %s
// REQUIRES: concurrency

// Module collision errors meant we couldn't add the stdio shims needed for linux
// and Android, see #43103.
// UNSUPPORTED: OS=linux-gnu, OS=linux-android, OS=linux-androideabi

// Regression test: stdin/stdout/stderr must be usable from concurrent contexts
// on most platforms, except linux and Android because of module errors. They are either
// computed vars marked nonisolated(unsafe)- get-only on OpenBSD, and get/set
// on FreeBSD- or imported const/let on WASI and Musl. Either way, try not to trip
// "global shared mutable state" errors in Swift 6 mode.

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#elseif canImport(WASILibc)
import WASILibc
#elseif os(Windows)
import CRT
#else
#error("Unsupported platform")
#endif

func useFromAsyncFunction() async {
    _ = stdin
    _ = stdout
    _ = stderr
}

actor StdioConsumer {
    func use() {
        _ = stdin
        _ = stdout
        _ = stderr
    }
}

nonisolated func useFromNonisolatedContext() {
    _ = stdin
    _ = stdout
    _ = stderr
}

// Make sure we can pass stdout/stderr to `FILE *` parameters of stdio functions
// imported from the same overlay.
func passToStdioFunctions() {
    setvbuf(stdout, nil, _IOLBF, 0)
    setvbuf(stderr, nil, _IOLBF, 0)
    fflush(stdout)
    fputs("hi", stderr)
}
