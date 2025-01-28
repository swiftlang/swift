//===--- Libc.swift - libc utility functions ------------------------------===//
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
// Since we are using C++ interop, some utility functions we've defined in
// modules/OS/Libc.h end up in a namespace.  This file moves them out again.
//
//===----------------------------------------------------------------------===//

internal import BacktracingImpl.OS.Libc

let _swift_open = swift.runtime.backtrace._swift_open
let _swift_get_errno = swift.runtime.backtrace._swift_get_errno
let _swift_set_errno = swift.runtime.backtrace._swift_set_errno
