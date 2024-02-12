//===--- AnsiColor.swift - ANSI formatting control codes ------------------===//
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
//  Provides ANSI support via Swift string interpolation.
//
//===----------------------------------------------------------------------===//

enum AnsiColor {
  case normal
  case black
  case red
  case green
  case yellow
  case blue
  case magenta
  case cyan
  case white
  case gray
  case brightRed
  case brightGreen
  case brightYellow
  case brightBlue
  case brightMagenta
  case brightCyan
  case brightWhite
  case rgb(r: Int, g: Int, b: Int)
  case grayscale(Int)

  var foregroundCode: String {
    switch self {
      case .normal:        return "39"
      case .black:         return "30"
      case .red:           return "31"
      case .green:         return "32"
      case .yellow:        return "33"
      case .blue:          return "34"
      case .cyan:          return "35"
      case .magenta:       return "36"
      case .white:         return "37"
      case .gray:          return "90"
      case .brightRed:     return "91"
      case .brightGreen:   return "92"
      case .brightYellow:  return "93"
      case .brightBlue:    return "94"
      case .brightCyan:    return "95"
      case .brightMagenta: return "96"
      case .brightWhite:   return "97"
      case let .rgb(r, g, b):
        let ndx = 16 + 36 * r + 6 * g + b
        return "38;5;\(ndx)"
      case let .grayscale(g):
        let ndx = 232 + g
        return "38;5;\(ndx)"
    }
  }

  var backgroundCode: String {
    switch self {
      case .normal:        return "49"
      case .black:         return "40"
      case .red:           return "41"
      case .green:         return "42"
      case .yellow:        return "43"
      case .blue:          return "44"
      case .cyan:          return "45"
      case .magenta:       return "46"
      case .white:         return "47"
      case .gray:          return "100"
      case .brightRed:     return "101"
      case .brightGreen:   return "102"
      case .brightYellow:  return "103"
      case .brightBlue:    return "104"
      case .brightCyan:    return "105"
      case .brightMagenta: return "106"
      case .brightWhite:   return "107"
      case let .rgb(r, g, b):
        let ndx = 16 + 36 * r + 6 * g + b
        return "48;5;\(ndx)"
      case let .grayscale(g):
        let ndx = 232 + g
        return "48;5;\(ndx)"
    }
  }
}

enum AnsiWeight {
  case normal
  case bold
  case faint

  var code: String {
    switch self {
      case .normal: return "22"
      case .bold: return "1"
      case .faint: return "2"
    }
  }
}

enum AnsiAttribute {
  case fg(AnsiColor)
  case bg(AnsiColor)
  case weight(AnsiWeight)
  case inverse(Bool)
}

extension DefaultStringInterpolation {
  mutating func appendInterpolation(ansi attrs: AnsiAttribute...) {
    var code = "\u{1b}["
    var first = true
    for attr in attrs {
      if first {
        first = false
      } else {
        code += ";"
      }

      switch attr {
        case let .fg(color):
          code += color.foregroundCode
        case let .bg(color):
          code += color.backgroundCode
        case let .weight(weight):
          code += weight.code
        case let .inverse(enabled):
          if enabled {
            code += "7"
          } else {
            code += "27"
          }
      }
    }
    code += "m"

    appendInterpolation(code)
  }

  mutating func appendInterpolation(fg: AnsiColor) {
    return appendInterpolation(ansi: .fg(fg))
  }
  mutating func appendInterpolation(bg: AnsiColor) {
    return appendInterpolation(ansi: .bg(bg))
  }
  mutating func appendInterpolation(weight: AnsiWeight) {
    return appendInterpolation(ansi: .weight(weight))
  }
  mutating func appendInterpolation(inverse: Bool) {
    return appendInterpolation(ansi: .inverse(inverse))
  }
}
