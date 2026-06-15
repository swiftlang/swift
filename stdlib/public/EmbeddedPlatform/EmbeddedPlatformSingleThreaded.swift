//===----------- EmbeddedPlatformSingleThreaded.swift ---------------------===//
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

@implementation @c
public func _swift_mutex_init(
  _ mutex: UnsafeMutableRawPointer,
  _ checked: CInt
) {}

@implementation @c
public func _swift_mutex_destroy(_ mutex: UnsafeMutableRawPointer) {}

@implementation @c
public func _swift_mutex_lock(_ mutex: UnsafeMutableRawPointer) {}

@implementation @c
public func _swift_mutex_unlock(_ mutex: UnsafeMutableRawPointer) {}

@implementation @c
public func _swift_mutex_tryLock(_ mutex: UnsafeMutableRawPointer) -> CInt {
  1
}
