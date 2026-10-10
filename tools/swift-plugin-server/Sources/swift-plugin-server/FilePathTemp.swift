/*
 This source file is part of the Swift System open source project

 Copyright (c) 2024 Apple Inc. and the Swift System project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
*/

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

// MARK: - API

/// Create a temporary path for the duration of the closure.
///
/// - Parameters:
///   - basename: The base name for the temporary path.
///   - body: The closure to execute.
///
/// Creates a temporary directory with a name based on the given `basename`,
/// executes `body`, passing in the path of the created directory, then
/// deletes the directory and all of its contents before returning.
func withTemporaryFilePath<R>(
  basename: FilePath.Component,
  _ body: (FilePath) throws -> R
) throws -> R {
  let temporaryDir = try createUniqueTemporaryDirectory(basename: basename)
  defer {
    try? _recursiveRemove(at: temporaryDir)
  }

  return try body(temporaryDir)
}

// MARK: - Internals

fileprivate let base64 = Array<UInt8>(
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".utf8
)

/// Create a directory that is only accessible to the current user.
///
/// - Parameters:
///   - path: The path of the directory to create.
/// - Returns: `true` if a new directory was created.
///
/// This function will throw if there is an error, except if the error
/// is that the directory exists, in which case it returns `false`.
fileprivate func makeLockedDownDirectory(at path: FilePath) throws -> Bool {
  return try path.withPlatformString {
    if mkdir($0, 0o700) == 0 {
      return true
    }
    let err = errno
    if err == Errno.fileExists.rawValue {
      return false
    } else {
      throw Errno(rawValue: err)
    }
  }
}

/// Generate a random string of base64 filename safe characters.
///
/// - Parameters:
///   - length: The number of characters in the returned string.
/// - Returns: A random string of length `length`.
fileprivate func createRandomString(length: Int) -> String {
  return String(
    decoding: (0..<length).map{
      _ in base64[Int.random(in: 0..<64)]
    },
    as: UTF8.self
  )
}

/// Given a base name, create a uniquely named temporary directory.
///
/// - Parameters:
///   - basename: The base name for the new directory.
/// - Returns: The path to the new directory.
///
/// Creates a directory in the system temporary directory whose name
/// starts with `basename`, followed by a `.` and then a random
/// string of characters.
func createUniqueTemporaryDirectory(
  basename: FilePath.Component
) throws -> FilePath {
  var tempDir = try _getTemporaryDirectory()
  tempDir.append(basename)

  while true {
    tempDir.extension = createRandomString(length: 16)

    if try makeLockedDownDirectory(at: tempDir) {
      return tempDir
    }
  }
}
