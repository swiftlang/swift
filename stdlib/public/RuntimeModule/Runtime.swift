//===--- Runtime.swift - Imports from libswiftCore ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Since we are using C++ interop, the imported APIs end up inside a namespace,
// which makes everything very verbose.  Define aliases in this file.
//
//===----------------------------------------------------------------------===//

import Swift

internal import BacktracingImpl.Runtime

typealias CrashInfo = swift.runtime.backtrace.CrashInfo

#if os(Linux)
typealias memserver_req = swift.runtime.backtrace.memserver_req
typealias memserver_resp = swift.runtime.backtrace.memserver_resp
typealias thread = swift.runtime.backtrace.thread
#endif
