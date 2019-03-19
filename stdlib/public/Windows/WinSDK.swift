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

// handleapi.h
public let INVALID_HANDLE_VALUE: HANDLE = HANDLE(bitPattern: -1)!

// shellapi.h
public let FOF_NO_UI: FILEOP_FLAGS =
    FILEOP_FLAGS(FOF_SILENT | FOF_NOCONFIRMATION | FOF_NOERRORUI | FOF_NOCONFIRMMKDIR)

// WinSock2.h
public let INVALID_SOCKET: SOCKET = SOCKET(bitPattern: -1)
public let FIONBIO: Int32 = 0x4667e

// WinUser.h
public let CW_USEDEFAULT: Int32 = Int32(truncatingIfNeeded: 2147483648)
public let WS_OVERLAPPEDWINDOW: UINT =
    UINT(WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX)
public let WS_POPUPWINDOW: UINT =
    UINT(Int32(WS_POPUP) | WS_BORDER | WS_SYSMENU)

// fileapi.h
public let INVALID_FILE_ATTRIBUTES: DWORD = DWORD(bitPattern: -1)

// CommCtrl.h
public let WC_BUTTONW: [WCHAR] = Array<WCHAR>("Button".utf16)
public let WC_COMBOBOXW: [WCHAR] = Array<WCHAR>("ComboBox".utf16)
public let WC_EDITW: [WCHAR] = Array<WCHAR>("Edit".utf16)
public let WC_HEADERW: [WCHAR] = Array<WCHAR>("SysHeader32".utf16)
public let WC_LISTBOXW: [WCHAR] = Array<WCHAR>("ListBox".utf16)
public let WC_LISTVIEWW: [WCHAR] = Array<WCHAR>("SysListView32".utf16)
public let WC_SCROLLBARW: [WCHAR] = Array<WCHAR>("ScrollBar".utf16)
public let WC_STATICW: [WCHAR] = Array<WCHAR>("Static".utf16)
public let WC_TABCONTROLW: [WCHAR] = Array<WCHAR>("SysTabControl32".utf16)
public let WC_TREEVIEWW: [WCHAR] = Array<WCHAR>("SysTreeView32".utf16)

public let ANIMATE_CLASSW: [WCHAR] = Array<WCHAR>("SysAnimate32".utf16)
public let HOTKEY_CLASSW: [WCHAR] = Array<WCHAR>("msctls_hotkey32".utf16)
public let PROGRESS_CLASSW: [WCHAR] = Array<WCHAR>("msctls_progress32".utf16)
public let STATUSCLASSNAMEW: [WCHAR] = Array<WCHAR>("msctls_statusbar32".utf16)
public let TOOLBARW_CLASSW: [WCHAR] = Array<WCHAR>("ToolbarWindow32".utf16)
public let TRACKBAR_CLASSW: [WCHAR] = Array<WCHAR>("msctls_trackbar32".utf16)
public let UPDOWN_CLASSW: [WCHAR] = Array<WCHAR>("msctls_updown32".utf16)

// Swift Convenience
public extension FILETIME {
  var time_t: time_t {
    let NTTime: Int64 = Int64(self.dwLowDateTime) | (Int64(self.dwHighDateTime) << 32)
    return (NTTime - 116444736000000000) / 10000000
  }

  init(from time: time_t) {
    let UNIXTime: Int64 = ((time * 10000000) + 116444736000000000)
    self = FILETIME(dwLowDateTime: DWORD(UNIXTime & 0xffffffff),
                    dwHighDateTime: DWORD((UNIXTime >> 32) & 0xffffffff))
  }
}

