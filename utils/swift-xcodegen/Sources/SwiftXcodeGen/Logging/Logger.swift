//===--- Logger.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation

public final class Logger: @unchecked Sendable {
  private let stateLock = Lock()
  private let outputLock = Lock()

  private var _hadError = false
  public var hadError: Bool {
    get { stateLock.withLock { _hadError } }
    set { stateLock.withLock { _hadError = newValue } }
  }

  private var _logLevel: LogLevel = .debug
  public var logLevel: LogLevel {
    get { stateLock.withLock { _logLevel } }
    set { stateLock.withLock { _logLevel = newValue } }
  }

  private var _useColor: Bool = true
  public var useColor: Bool {
    get { stateLock.withLock { _useColor } }
    set { stateLock.withLock { _useColor = newValue } }
  }

  private var _output: LoggableStream?
  public var output: LoggableStream? {
    get { stateLock.withLock { _output } }
    set { stateLock.withLock { _output = newValue } }
  }

  public init() {}
}

extension Logger {
  public enum LogLevel: Comparable {
    /// A message with information that isn't useful to the user, but is
    /// useful when debugging issues.
    case debug

    /// A message with mundane information that may be useful to know if
    /// you're interested in verbose output, but is otherwise unimportant.
    case info

    /// A message with information that does not require any intervention from
    /// the user, but is nonetheless something they may want to be aware of.
    case note

    /// A message that describes an issue that ought to be resolved by the
    /// user, but still allows the program to exit successfully.
    case warning

    /// A message that describes an issue where the program cannot exit
    /// successfully.
    case error
  }

  private func log(_ message: @autoclosure () -> String, level: LogLevel) {
    guard level >= logLevel else { return }
    let output = self.output ?? FileHandleStream.stderr
    let useColor = self.useColor && output.supportsColor
    outputLock.withLock {
      level.write(to: output, useColor: useColor)
      output.write(": \(message())\n")
    }
  }

  public func debug(_ message: @autoclosure () -> String) {
    log(message(), level: .debug)
  }

  public func info(_ message: @autoclosure () -> String) {
    log(message(), level: .info)
  }

  public func note(_ message: @autoclosure () -> String) {
    log(message(), level: .note)
  }

  public func warning(_ message: @autoclosure () -> String) {
    log(message(), level: .warning)
  }

  public func error(_ message: @autoclosure () -> String) {
    hadError = true
    log(message(), level: .error)
  }
}

public protocol Loggable {
  func write(to stream: LoggableStream, useColor: Bool)
}

extension Logger.LogLevel: Loggable, CustomStringConvertible {
  public var description: String {
    switch self {
    case .debug:   "debug"
    case .info:    "info"
    case .note:    "note"
    case .warning: "warning"
    case .error:   "error"
    }
  }
  private var ansiColor: AnsiColor {
    switch self {
    case .debug:   .magenta
    case .info:    .blue
    case .note:    .brightCyan
    case .warning: .brightYellow
    case .error:   .brightRed
    }
  }
  public func write(to stream: LoggableStream, useColor: Bool) {
    let str = useColor 
      ? "\(fg: ansiColor)\(weight: .bold)\(self)\(fg: .normal)\(weight: .normal)"
      : "\(self)"
    stream.write(str)
  }
}

public protocol LoggableStream: Sendable {
  var supportsColor: Bool { get }
  func write(_: String)
}

/// Check whether $TERM supports color. Ideally we'd consult terminfo, but
/// there aren't any particularly nice APIs for that in the SDK AFAIK. We could
/// shell out to tput, but that adds ~100ms of overhead which I don't think is
/// worth it. This simple check (taken from LLVM) is good enough for now.
fileprivate let termSupportsColor: Bool = {
  guard let termEnv = getenv("TERM") else { return false }
  switch String(cString: termEnv) {
  case "ansi", "cygwin", "linux":
    return true
  case let term where
    term.hasPrefix("screen") ||
    term.hasPrefix("xterm") ||
    term.hasPrefix("vt100") ||
    term.hasPrefix("rxvt") ||
    term.hasSuffix("color"):
    return true
  default:
    return false
  }
}()

public struct FileHandleStream: LoggableStream, @unchecked Sendable {
  public let handle: UnsafeMutablePointer<FILE>
  public let supportsColor: Bool

  public init(_ handle: UnsafeMutablePointer<FILE>) {
    self.handle = handle
    self.supportsColor = isatty(fileno(handle)) != 0 && termSupportsColor
  }
  public func write(_ string: String) {
    fputs(string, handle)
  }
}

extension FileHandleStream {
  static let stdout = Self(Darwin.stdout)
  static let stderr = Self(Darwin.stderr)
}

public let log = Logger()
