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

internal func getRemoteProcess(processId: ProcessIdentifier,
                               options: UniversalOptions) -> (any RemoteProcess)? {
  return DarwinRemoteProcess(processId: processId,
                             forkCorpse: options.forkCorpse)
}

internal func getProcessName(processId: ProcessIdentifier) -> String? {
  var info = proc_bsdinfo()
  let bsdinfoSize = Int32(MemoryLayout<proc_bsdinfo>.stride)
  let size = proc_pidinfo(processId, PROC_PIDTBSDINFO, 0, &info, bsdinfoSize)
  if (size != bsdinfoSize) {
    return nil
  }
 let processName = withUnsafeBytes(of: info.pbi_name) { buffer in
    let nonnullBuffer = buffer.prefix { $0 != 0 }
    return String(decoding: nonnullBuffer, as: UTF8.self)
  }
  return processName
}

internal func getAllProcesses(options: UniversalOptions) -> [ProcessIdentifier]? {
  var ProcessIdentifiers = [ProcessIdentifier]()
  let kinfo_stride = MemoryLayout<kinfo_proc>.stride
  var bufferSize: Int = 0
  var name: [Int32] = [CTL_KERN, KERN_PROC, KERN_PROC_ALL]

  guard sysctl(&name, u_int(name.count), nil, &bufferSize, nil, 0) == 0 else {
    return nil
  }
  let count = bufferSize / kinfo_stride
  var buffer = Array(repeating: kinfo_proc(), count: count)
  guard sysctl(&name, u_int(name.count), &buffer, &bufferSize, nil, 0) == 0 else {
    return nil
  }
  let newCount = bufferSize / kinfo_stride
  if count > newCount {
    buffer.removeLast(count - newCount)
  }
  let sorted = buffer.sorted { first, second in
    first.kp_proc.p_pid > second.kp_proc.p_pid
  }
  let myPid = getpid()
  for kinfo in sorted {
    let pid = kinfo.kp_proc.p_pid
    if pid <= 1 {
      break
    }
    if pid == myPid { // skip self
      continue
    }
    ProcessIdentifiers.append(pid)
  }
  return ProcessIdentifiers
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

internal func getRemoteProcess(processId: ProcessIdentifier,
                               options: UniversalOptions) -> (any RemoteProcess)? {
  return WindowsRemoteProcess(processId: processId)
}

#elseif os(Linux)
import Foundation

internal typealias ProcessIdentifier = LinuxRemoteProcess.ProcessIdentifier

internal func process(matching: String) -> ProcessIdentifier? {
  guard let processId = LinuxRemoteProcess.ProcessIdentifier(matching) else {
    return nil
  }

  let procfs_path = "/proc/\(processId)"
  var isDirectory: Bool = false
  guard FileManager.default.fileExists(atPath: procfs_path, isDirectory: &isDirectory)
        && isDirectory else {
    return nil
  }

  return processId
}

internal func getRemoteProcess(processId: ProcessIdentifier,
                               options: UniversalOptions) -> (any RemoteProcess)? {
  return LinuxRemoteProcess(processId: processId)
}

#elseif os(Android)
import Foundation

internal typealias ProcessIdentifier = AndroidRemoteProcess.ProcessIdentifier

internal func process(matching: String) -> ProcessIdentifier? {
  guard let processId = AndroidRemoteProcess.ProcessIdentifier(matching) else {
    return nil
  }

  let procfsPath = "/proc/\(processId)"
  var isDirectory: Bool = false
  guard FileManager.default.fileExists(atPath: procfsPath, isDirectory: &isDirectory)
        && isDirectory else {
    return nil
  }

  return processId
}

internal func getRemoteProcess(processId: ProcessIdentifier,
                               options: UniversalOptions) -> (any RemoteProcess)? {
  return AndroidRemoteProcess(processId: processId)
}

#else
#error("Unsupported platform")
#endif
