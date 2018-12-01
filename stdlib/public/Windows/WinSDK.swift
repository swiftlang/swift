//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import WinSDK // Clang module

// WinBase.h
public let HANDLE_FLAG_INHERIT: DWORD = 0x00000001

// WinBase.h
public let STARTF_USESTDHANDLES: DWORD = 0x00000100

// WinBase.h
public let INFINITE: DWORD = DWORD(bitPattern: -1)

// WinBase.h
public let WAIT_OBJECT_0: DWORD = 0

// minwindef.h
public let FALSE: BOOL = 0

// minwindef.h
public let TRUE: BOOL = 1

