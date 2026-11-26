// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

internal import WinSDK

@_transparent
internal var IMAGE_DIRECTORY_ENTRY_IMPORT: USHORT {
  USHORT(WinSDK.IMAGE_DIRECTORY_ENTRY_IMPORT)
}

@_transparent
internal var IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT: USHORT {
  USHORT(WinSDK.IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT)
}
