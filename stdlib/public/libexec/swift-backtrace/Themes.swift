//===--- Themes.swift - Represents a process we are inspecting ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines the `Theme` struct that we use for color support.
//
//===----------------------------------------------------------------------===//

@_spi(Formatting) import _Backtracing

protocol ErrorAndWarningTheme {
  func crashReason(_ s: String) -> String
  func error(_ s: String) -> String
  func warning(_ s: String) -> String
  func info(_ s: String) -> String
}

extension ErrorAndWarningTheme {
  public func crashReason(_ s: String) -> String { return "*** \(s) ***" }
  public func error(_ s: String) -> String { return   "!!! error: \(s)" }
  public func warning(_ s: String) -> String { return "/!\\ warning: \(s)" }
  public func info(_ s: String) -> String { return "(i) \(s)" }
}

protocol PromptTheme {
  func prompt(_ s: String) -> String
}

extension PromptTheme {
  public func prompt(_ s: String) -> String { return s }
}

protocol MemoryDumpTheme {
  func address(_ s: String) -> String
  func data(_ s: String) -> String
  func printable(_ s: String) -> String
  func nonPrintable(_ s: String) -> String
}

extension MemoryDumpTheme {
  public func address(_ s: String) -> String { return s }
  public func data(_ s: String) -> String { return s }
  public func printable(_ s: String) -> String { return s }
  public func nonPrintable(_ s: String) -> String { return s }
}

protocol RegisterDumpTheme : MemoryDumpTheme {
  func register(_ s: String) -> String
  func hexValue(_ s: String) -> String
  func decimalValue(_ s: String) -> String
  func flags(_ s: String) -> String
}

extension RegisterDumpTheme {
  public func register(_ s: String) -> String { return s }
  public func hexValue(_ s: String) -> String { return s }
  public func decimalValue(_ s: String) -> String { return s }
  public func flags(_ s: String) -> String { return s }
}

typealias Theme = BacktraceFormattingTheme & ErrorAndWarningTheme &
  PromptTheme & MemoryDumpTheme & RegisterDumpTheme

enum Themes {

  struct Plain: Theme {
  }

  struct Color: Theme {
    public func frameIndex(_ s: String) -> String {
      return "\(fg: .gray)\(s)\(fg: .normal)"
    }
    public func programCounter(_ s: String) -> String {
      return "\(fg: .green)\(s)\(fg: .normal)"
    }
    public func frameAttribute(_ s: String) -> String {
      return "\(fg: .blue)[\(s)]\(fg: .normal)"
    }

    public func symbol(_ s: String) -> String {
      return "\(fg: .brightMagenta)\(s)\(fg: .normal)"
    }
    public func offset(_ s: String) -> String {
      return "\(fg: .white)\(s)\(fg: .normal)"
    }
    public func sourceLocation(_ s: String) -> String {
      return "\(fg: .yellow)\(s)\(fg: .normal)"
    }
    public func lineNumber(_ s: String) -> String {
      return "\(fg: .gray)\(s)\(fg: .normal)"
    }
    public func code(_ s: String) -> String {
      return "\(s)"
    }
    public func crashedLine(_ s: String) -> String {
      return "\(bg: .grayscale(2))\(s)\(bg: .normal)"
    }
    public func crashLocation(_ s: String) -> String {
      return "\(fg: .brightRed)\(s)\(fg: .normal)"
    }
    public func imageName(_ s: String) -> String {
      return "\(fg: .cyan)\(s)\(fg: .normal)"
    }
    public func imageAddressRange(_ s: String) -> String {
      return "\(fg: .green)\(s)\(fg: .normal)"
    }
    public func imageBuildID(_ s: String) -> String {
      return "\(fg: .white)\(s)\(fg: .normal)"
    }
    public func imagePath(_ s: String) -> String {
      return "\(fg: .gray)\(s)\(fg: .normal)"
    }

    public func prompt(_ s: String) -> String {
      return "\(fg: .gray)\(s)\(fg: .normal)"
    }

    public func address(_ s: String) -> String {
      return "\(fg: .green)\(s)\(fg: .normal)"
    }
    public func data(_ s: String) -> String {
      return s
    }
    public func printable(_ s: String) -> String {
      return s
    }
    public func nonPrintable(_ s: String) -> String {
      return "\(fg: .gray)\(s)\(fg: .normal)"
    }

    public func register(_ s: String) -> String {
      return s
    }
    public func hexValue(_ s: String) -> String {
      return "\(fg: .green)\(s)\(fg: .normal)"
    }
    public func decimalValue(_ s: String) -> String {
      return "\(fg: .green)\(s)\(fg: .normal)"
    }
    public func flags(_ s: String) -> String {
      return "\(fg: .magenta)\(s)\(fg: .normal)"
    }

    public func crashReason(_ s: String) -> String {
      return "💣 \(fg: .brightRed)\(s)\(fg: .normal)"
    }
    public func error(_ s: String) -> String {
      return "🛑 \(fg: .brightRed)error: \(s)\(fg: .normal)"
    }
    public func warning(_ s: String) -> String {
      return "⚠️ \(fg: .brightYellow)warning: \(s)\(fg: .normal)"
    }
    public func info(_ s: String) -> String {
      return "ℹ️ \(s)"
    }
  }

  static let plain = Plain()
  static let color = Color()

}
