//===-- PythonLibrary.swift -----------------------------------*- swift -*-===//
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
//
// This file implements the logic for dynamically loading Python at runtime.
//
//===----------------------------------------------------------------------===//

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif os(Windows)
import MSVCRT
import WinSDK
#endif

//===----------------------------------------------------------------------===//
// The `PythonLibrary` struct that loads Python symbols at runtime.
//===----------------------------------------------------------------------===//

public struct PythonLibrary {
  private static let shared = PythonLibrary()
  private static let pythonLegacySymbolName = "PyString_AsString"

  private let pythonLibraryHandle: UnsafeMutableRawPointer
  private let isLegacyPython: Bool

  private init() {
    guard let pythonLibraryHandle = PythonLibrary.loadPythonLibrary() else {
      fatalError("""
        Python library not found. Set the \(Environment.library.key) \
        environment variable with the path to a Python library.
        """)
    }
    self.pythonLibraryHandle = pythonLibraryHandle

    // Check if Python is legacy (Python 2)
    isLegacyPython = PythonLibrary.loadSymbol(
      pythonLibraryHandle,
      PythonLibrary.pythonLegacySymbolName) != nil
    if isLegacyPython {
      PythonLibrary.log(
        "Loaded legacy Python library, using legacy symbols...")
    }
  }

  static func loadSymbol(
    _ libraryHandle: UnsafeMutableRawPointer, _ name: String
    ) -> UnsafeMutableRawPointer? {
    #if canImport(Darwin) || canImport(Glibc)
    return dlsym(libraryHandle, name)
    #elseif os(Windows)
    let moduleHandle = libraryHandle
      .assumingMemoryBound(to: HINSTANCE__.self)
    let moduleSymbol = GetProcAddress(moduleHandle, name)
    return unsafeBitCast(moduleSymbol, to: UnsafeMutableRawPointer?.self)
    #endif
  }

  static func loadSymbol<T>(
    name: String, legacyName: String? = nil, type: T.Type = T.self
    ) -> T {
    var name = name
    if let legacyName = legacyName, PythonLibrary.shared.isLegacyPython {
      name = legacyName
    }

    log("Loading symbol '\(name)' from the Python library...")
    return unsafeBitCast(
      loadSymbol(PythonLibrary.shared.pythonLibraryHandle, name),
      to: type
    )
  }
}

// Methods of `PythonLibrary` required to set a given the Python version.
public extension PythonLibrary {
  static func useVersion(_ major: Int, _ minor: Int? = nil) {
    let version = PythonVersion(major: major, minor: minor)
    PythonLibrary.Environment.version.set(version.versionString)
  }
}

// `PythonVersion` struct that defines a given Python version.
private extension PythonLibrary {
  struct PythonVersion {
    let major: Int
    let minor: Int?

    static let versionSeparator: Character = "."

    init(major: Int, minor: Int?) {
      self.major = major
      self.minor = minor
    }

    var versionString: String {
      var versionString = String(major)
      if let minor = minor {
        versionString += "\(PythonVersion.versionSeparator)\(minor)"
      }
      return versionString
    }
  }
}

// `PythonLibrary.Environment` enum used to read and set environment variables.
private extension PythonLibrary {
  enum Environment: String {
    private static let keyPrefix = "PYTHON"
    private static let keySeparator = "_"

    case library = "LIBRARY"
    case version = "VERSION"
    case loaderLogging = "LOADER_LOGGING"

    var key: String {
      return Environment.keyPrefix + Environment.keySeparator + rawValue
    }

    var value: String? {
      guard let value = getenv(key) else { return nil }
      return String(cString: value)
    }

    func set(_ value: String) {
      #if canImport(Darwin) || canImport(Glibc)
      setenv(key, value, 1)
      #elseif os(Windows)
      _putenv_s(key, value)
      #endif
    }
  }
}

// Methods of `PythonLibrary` required to load the Python library.
private extension PythonLibrary {
  static let supportedMajorVersions: [Int] = [3, 2]
  static let supportedMinorVersions: [Int] = Array(0...30).reversed()

  static let libraryPathVersionCharacter: Character = ":"

  #if canImport(Darwin)
  static var libraryNames = ["Python.framework/Versions/:/Python"]
  static var libraryPathExtensions = [""]
  static var librarySearchPaths = ["", "/usr/local/Frameworks/"]
  static var libraryVersionSeparator = "."
  #elseif os(Linux)
  static var libraryNames = ["libpython:", "libpython:m"]
  static var libraryPathExtensions = [".so"]
  static var librarySearchPaths = [""]
  static var libraryVersionSeparator = "."
  #elseif os(Windows)
  static var libraryNames = ["python:"]
  static var libraryPathExtensions = [".dll"]
  static var librarySearchPaths = [""]
  static var libraryVersionSeparator = ""
  #endif

  static let libraryPaths: [String] = {
    var libraryPaths: [String] = []
    for librarySearchPath in librarySearchPaths {
      for libraryName in libraryNames {
        for libraryPathExtension in libraryPathExtensions {
          let libraryPath =
            librarySearchPath + libraryName + libraryPathExtension
          libraryPaths.append(libraryPath)
        }
      }
    }
    return libraryPaths
  }()

  static func loadPythonLibrary() -> UnsafeMutableRawPointer? {
    if let pythonLibraryPath = Environment.library.value {
      return loadPythonLibrary(at: pythonLibraryPath)
    }

    for majorVersion in supportedMajorVersions {
      for minorVersion in supportedMinorVersions {
        for libraryPath in libraryPaths {
          let version = PythonVersion(major: majorVersion, minor: minorVersion)
          guard let pythonLibraryHandle = loadPythonLibrary(
            at: libraryPath, version: version) else {
              continue
          }
          return pythonLibraryHandle
        }
      }
    }
    return nil
  }

  static func loadPythonLibrary(
    at path: String, version: PythonVersion
    ) -> UnsafeMutableRawPointer? {
    let versionString = version.versionString

    if let requiredPythonVersion = Environment.version.value {
      let requiredMajorVersion = Int(requiredPythonVersion)
      if requiredPythonVersion != versionString,
        requiredMajorVersion != version.major {
        return nil
      }
    }

    let libraryVersionString = versionString
      .split(separator: PythonVersion.versionSeparator)
      .joined(separator: libraryVersionSeparator)
    let path = path.split(separator: libraryPathVersionCharacter)
      .joined(separator: libraryVersionString)
    return loadPythonLibrary(at: path)
  }

  static func loadPythonLibrary(at path: String) -> UnsafeMutableRawPointer? {
    log("Trying to load library at '\(path)'...")
    #if canImport(Darwin) || canImport(Glibc)
    // Must be RTLD_GLOBAL because subsequent .so files from the imported python
    // modules may depend on this .so file.
    let pythonLibraryHandle = dlopen(path, RTLD_LAZY | RTLD_GLOBAL)
    #elseif os(Windows)
    let pythonLibraryHandle = UnsafeMutableRawPointer(LoadLibraryA(path))
    #endif

    if pythonLibraryHandle != nil {
      log("Library at '\(path)' was sucessfully loaded.")
    }
    return pythonLibraryHandle
  }
}

// Methods of `PythonLibrary` used for logging messages.
private extension PythonLibrary {
  static func log(_ message: String) {
    guard Environment.loaderLogging.value != nil else { return }
    fputs(message + "\n", stderr)
  }
}
