import _CompilerPluginSupport

struct StringifyMacro: _CompilerPlugin {
  static func _name() -> (UnsafePointer<UInt8>, count: Int) {
    var name = "customStringify"
    return name.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _genericSignature() -> (UnsafePointer<UInt8>?, count: Int) {
    var genSig = "<T>"
    return genSig.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _typeSignature() -> (UnsafePointer<UInt8>, count: Int) {
    var typeSig = "(T) -> (T, String)"
    return typeSig.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _owningModule() -> (UnsafePointer<UInt8>, count: Int) {
    var swiftModule = "Swift"
    return swiftModule.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _supplementalSignatureModules() -> (UnsafePointer<UInt8>, count: Int) {
    var nothing = ""
    return nothing.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _kind() -> _CompilerPluginKind {
    .expressionMacro
  }

  static func _rewrite(
    targetModuleName: UnsafePointer<UInt8>,
    targetModuleNameCount: Int,
    filePath: UnsafePointer<UInt8>,
    filePathCount: Int,
    sourceFileText: UnsafePointer<UInt8>,
    sourceFileTextCount: Int,
    localSourceText: UnsafePointer<UInt8>,
    localSourceTextCount: Int
  ) -> (UnsafePointer<UInt8>?, count: Int) {
    let meeTextBuffer = UnsafeBufferPointer(
      start: localSourceText, count: localSourceTextCount)
    let meeText = String(decoding: meeTextBuffer, as: UTF8.self)
    let prefix = "#customStringify("
    guard meeText.starts(with: prefix), meeText.last == ")" else {
      return (nil, 0)
    }
    let expr = meeText.dropFirst(prefix.count).dropLast()
    // Or use regex...
    //   let match = meeText.firstMatch {
    //     "#customStringify"
    //     ZeroOrMore {
    //       CharacterClass(.whitespace, .newlineSequence)
    //     }
    //     "("
    //     Capture(OneOrMore(.any))
    //     ")"
    //     ZeroOrMore {
    //       CharacterClass(.whitespace, .newlineSequence)
    //     }
    //   }
    //   guard let expr = match?.1 else {
    //     return (nil, count: 0)
    //   }

    var resultString = "(\(expr), #\"\(expr)\"#)"
    return resultString.withUTF8 { buffer in
      let result = UnsafeMutableBufferPointer<UInt8>.allocate(
          capacity: buffer.count + 1)
      _ = result.initialize(from: buffer)
      result[buffer.count] = 0
      return (UnsafePointer(result.baseAddress), buffer.count)
    }
  }
}

struct ColorLiteralMacro: _CompilerPlugin {
  static func _name() -> (UnsafePointer<UInt8>, count: Int) {
    var name = "customColorLiteral"
    return name.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _genericSignature() -> (UnsafePointer<UInt8>?, count: Int) {
    var genSig = "<T: _ExpressibleByColorLiteral>"
    return genSig.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _typeSignature() -> (UnsafePointer<UInt8>, count: Int) {
    var typeSig =
      """
      (
       red red: Float, green green: Float, blue blue: Float, alpha alpha: Float
      ) -> T
      """
    return typeSig.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _owningModule() -> (UnsafePointer<UInt8>, count: Int) {
    var swiftModule = "ColorLib"
    return swiftModule.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _supplementalSignatureModules() -> (UnsafePointer<UInt8>, count: Int) {
    var nothing = ""
    return nothing.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _kind() -> _CompilerPluginKind {
    .expressionMacro
  }

  static func _rewrite(
    targetModuleName: UnsafePointer<UInt8>,
    targetModuleNameCount: Int,
    filePath: UnsafePointer<UInt8>,
    filePathCount: Int,
    sourceFileText: UnsafePointer<UInt8>,
    sourceFileTextCount: Int,
    localSourceText: UnsafePointer<UInt8>,
    localSourceTextCount: Int
  ) -> (UnsafePointer<UInt8>?, count: Int) {
    let meeTextBuffer = UnsafeBufferPointer(
      start: localSourceText, count: localSourceTextCount)
    let meeText = String(decoding: meeTextBuffer, as: UTF8.self)
    let prefix = "#customColorLiteral(red:"
    guard meeText.starts(with: prefix), meeText.last == ")" else {
      return (nil, 0)
    }
    let expr = meeText.dropFirst(prefix.count).dropLast()
    var resultString = ".init(_colorLiteralRed:\(expr))"
    return resultString.withUTF8 { buffer in
      let result = UnsafeMutableBufferPointer<UInt8>.allocate(
          capacity: buffer.count + 1)
      _ = result.initialize(from: buffer)
      result[buffer.count] = 0
      return (UnsafePointer(result.baseAddress), buffer.count)
    }
  }
}

struct HSVColorLiteralMacro: _CompilerPlugin {
  static func _name() -> (UnsafePointer<UInt8>, count: Int) {
    var name = "customColorLiteral"
    return name.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _genericSignature() -> (UnsafePointer<UInt8>?, count: Int) {
    return (nil, count: 0)
  }

  static func _typeSignature() -> (UnsafePointer<UInt8>, count: Int) {
    var typeSig =
      """
      (
       hue hue: Float, saturation saturation: Float, value value: Float
      ) -> MyColor
      """
    return typeSig.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _owningModule() -> (UnsafePointer<UInt8>, count: Int) {
    var swiftModule = "ColorLib"
    return swiftModule.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _supplementalSignatureModules() -> (UnsafePointer<UInt8>, count: Int) {
    var nothing = ""
    return nothing.withUTF8 { buffer in
      let result = UnsafeMutablePointer<UInt8>.allocate(capacity: buffer.count)
      result.initialize(from: buffer.baseAddress!, count: buffer.count)
      return (UnsafePointer(result), count: buffer.count)
    }
  }

  static func _kind() -> _CompilerPluginKind {
    .expressionMacro
  }

  static func _rewrite(
    targetModuleName: UnsafePointer<UInt8>,
    targetModuleNameCount: Int,
    filePath: UnsafePointer<UInt8>,
    filePathCount: Int,
    sourceFileText: UnsafePointer<UInt8>,
    sourceFileTextCount: Int,
    localSourceText: UnsafePointer<UInt8>,
    localSourceTextCount: Int
  ) -> (UnsafePointer<UInt8>?, count: Int) {
    let meeTextBuffer = UnsafeBufferPointer(
      start: localSourceText, count: localSourceTextCount)
    let meeText = String(decoding: meeTextBuffer, as: UTF8.self)
    let prefix = "#customColorLiteral(hue:"
    guard meeText.starts(with: prefix), meeText.last == ")" else {
      return (nil, 0)
    }
    let expr = meeText.dropFirst(prefix.count).dropLast()
    var resultString = ".init(_colorLiteralHue:\(expr))"
    return resultString.withUTF8 { buffer in
      let result = UnsafeMutableBufferPointer<UInt8>.allocate(
          capacity: buffer.count + 1)
      _ = result.initialize(from: buffer)
      result[buffer.count] = 0
      return (UnsafePointer(result.baseAddress), buffer.count)
    }
  }
}


public var allMacros: [Any.Type] {
  [
    StringifyMacro.self,
    ColorLiteralMacro.self,
    HSVColorLiteralMacro.self
  ]
}
