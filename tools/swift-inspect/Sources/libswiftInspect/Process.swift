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

#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
import Darwin

internal typealias ProcessIdentifier = DarwinRemoteProcess.ProcessIdentifier

internal func process(matching: String) -> ProcessIdentifier? {
  if refersToSelf(matching) {
    return getpid()
  } else {
    return pidFromHint(matching)
  }
}

private func refersToSelf(_ str: String) -> Bool {
  guard let myPath = CommandLine.arguments.first else {
    return false
  }

  // If string matches the full path, success.
  if myPath == str {
    return true
  }

  // If there's a slash in the string, compare with the component following the
  // slash.
  if let slashIndex = myPath.lastIndex(of: "/") {
    let myName = myPath[slashIndex...].dropFirst()
    return myName == str
  }

  // No match.
  return false
}
#elseif os(Windows)
import WinSDK

internal typealias ProcessIdentifier = WindowsRemoteProcess.ProcessIdentifier

internal func process(matching: String) -> ProcessIdentifier? {
  if let dwProcess = DWORD(matching) {
    return dwProcess
  }

  let hSnapshot = CreateToolhelp32Snapshot(DWORD(TH32CS_SNAPPROCESS), 0)
  if hSnapshot == INVALID_HANDLE_VALUE {
    return nil
  }
  defer { CloseHandle(hSnapshot) }

  var entry: PROCESSENTRY32W = PROCESSENTRY32W()
  entry.dwSize = DWORD(MemoryLayout<PROCESSENTRY32W>.size)

  if !Process32FirstW(hSnapshot, &entry) {
    return nil
  }

  var matches: [(ProcessIdentifier, String)] = []
  repeat {
    let executable: String = withUnsafePointer(to: entry.szExeFile) {
      $0.withMemoryRebound(to: WCHAR.self,
                           capacity: MemoryLayout.size(ofValue: $0) / MemoryLayout<WCHAR>.size) {
        String(decodingCString: $0, as: UTF16.self)
      }
    }
    if executable.hasPrefix(matching) {
      matches.append((entry.th32ProcessID, executable))
    }
  } while Process32NextW(hSnapshot, &entry)

  return matches.first?.0
}
#else
#error("Unsupported platform")
#endif
