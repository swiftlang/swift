/*
 This source file is part of the Swift System open source project

 Copyright (c) 2024 Apple Inc. and the Swift System project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 */

#if !os(Windows)

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#elseif canImport(WASILibc)
import WASILibc
#else
#error("Unsupported platform")
#endif

import SystemPackage

/// Get the path to the system temporary directory.
internal func _getTemporaryDirectory() throws -> FilePath {
  guard let tmp = getenv("TMPDIR") else {
    return "/tmp"
  }

  return FilePath(stringLiteral: .init(cString: tmp))
}

/// Delete the entire contents of a directory, including its subdirectories.
///
/// - Parameters:
///   - path: The directory to be deleted.
///
/// Removes a directory completely, including all of its contents.
internal func _recursiveRemove(
  at path: FilePath
) throws {
  let dirfd = try FileDescriptor.open(path, .readOnly, options: .directory)
  defer {
    try? dirfd.close()
  }

  let dot: (CInterop.PlatformChar, CInterop.PlatformChar) = (46, 0)
  try withUnsafeBytes(of: dot) {
    try recursiveRemove(
      in: dirfd.rawValue,
      name: $0.assumingMemoryBound(to: CInterop.PlatformChar.self).baseAddress!
    )
  }

  try path.withPlatformString {
    let error = rmdir($0)
    if error != 0 {
      throw Errno(rawValue: error)
    }
  }
}

internal let SYSTEM_AT_REMOVE_DIR = AT_REMOVEDIR
internal let SYSTEM_DT_DIR = DT_DIR
internal typealias system_dirent = dirent
#if os(Linux) || os(Android)
internal typealias system_DIRPtr = OpaquePointer
#else
internal typealias system_DIRPtr = UnsafeMutablePointer<DIR>
#endif


/// Open a directory by reference to its parent and name.
///
/// - Parameters:
///   - dirfd: An open file descriptor for the parent directory.
///   - name: The name of the directory to open.
/// - Returns: A pointer to a `DIR` structure.
///
/// This is like `opendir()`, but instead of taking a path, it uses a
/// file descriptor pointing at the parent, thus avoiding path length
/// limits.
fileprivate func impl_opendirat(
  _ dirfd: CInt,
  _ name: UnsafePointer<CInterop.PlatformChar>
) -> system_DIRPtr? {
  let fd = openat(dirfd, name,
                         FileDescriptor.AccessMode.readOnly.rawValue
                           | FileDescriptor.OpenOptions.directory.rawValue)
  if fd < 0 {
    return nil
  }
  return fdopendir(fd)
}

/// Invoke a closure for each file within a particular directory.
///
/// - Parameters:
///   - dirfd: The parent of the directory to be enumerated.
///   - subdir: The subdirectory to be enumerated.
///   - body: The closure that will be invoked.
///
/// We skip the `.` and `..` pseudo-entries.
fileprivate func forEachFile(
  in dirfd: CInt,
  subdir: UnsafePointer<CInterop.PlatformChar>,
  _ body: (system_dirent) throws -> ()
) throws {
  guard let dir = impl_opendirat(dirfd, subdir) else {
    throw Errno(rawValue: errno)
  }
  defer {
    _ = closedir(dir)
  }

  while let dirent = readdir(dir) {
    // Skip . and ..
    if dirent.pointee.d_name.0 == 46
         && (dirent.pointee.d_name.1 == 0
               || (dirent.pointee.d_name.1 == 46
                     && dirent.pointee.d_name.2 == 0)) {
      continue
    }

    try body(dirent.pointee)
  }
}

/// Delete the entire contents of a directory, including its subdirectories.
///
/// - Parameters:
///   - dirfd: The parent of the directory to be removed.
///   - name: The name of the directory to be removed.
///
/// Removes a directory completely, including all of its contents.
fileprivate func recursiveRemove(
  in dirfd: CInt,
  name: UnsafePointer<CInterop.PlatformChar>
) throws {
  // First, deal with subdirectories
  try forEachFile(in: dirfd, subdir: name) { dirent in
    if dirent.d_type == SYSTEM_DT_DIR {
      try withUnsafeBytes(of: dirent.d_name) {
        try recursiveRemove(
          in: dirfd,
          name: $0.assumingMemoryBound(to: CInterop.PlatformChar.self)
            .baseAddress!
        )
      }
    }
  }

  // Now delete the contents of this directory
  try forEachFile(in: dirfd, subdir: name) { dirent in
    let flag: CInt

    if dirent.d_type == SYSTEM_DT_DIR {
      flag = SYSTEM_AT_REMOVE_DIR
    } else {
      flag = 0
    }

    let result = withUnsafeBytes(of: dirent.d_name) {
      unlinkat(dirfd, $0.assumingMemoryBound(to: CInterop.PlatformChar.self).baseAddress!, flag)
    }

    if result != 0 {
      throw Errno(rawValue: errno)
    }
  }
}

#endif // !os(Windows)

