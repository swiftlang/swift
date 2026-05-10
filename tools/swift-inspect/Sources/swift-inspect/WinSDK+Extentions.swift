//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(Windows)

import WinSDK

internal var FILE_MAP_ALL_ACCESS: DWORD {
  DWORD(STANDARD_RIGHTS_REQUIRED | SECTION_QUERY |
        SECTION_MAP_WRITE | SECTION_MAP_READ | SECTION_MAP_EXECUTE |
        SECTION_EXTEND_SIZE)
}

internal func CreateEvent(_ name: String, bManualReset: Bool = false,
                          bInitialState: Bool = false) -> HANDLE? {
  let hEvent: HANDLE = CreateEventA(LPSECURITY_ATTRIBUTES(bitPattern: 0),
                                    bManualReset, bInitialState, name)
  if hEvent == HANDLE(bitPattern: 0) {
    print("CreateEvent failed \(GetLastError())")
    return nil
  }
  return hEvent
}

#endif
