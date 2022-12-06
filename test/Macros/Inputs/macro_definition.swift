import _CompilerPluginSupport

public struct StringifyMacro: _CompilerPlugin {
  public static func _kind() -> _CompilerPluginKind {
    .expressionMacro
  }

  public static func _rewrite(
    targetModuleName: UnsafePointer<UInt8>,
    targetModuleNameCount: Int,
    filePath: UnsafePointer<UInt8>,
    filePathCount: Int,
    sourceFileText: UnsafePointer<UInt8>,
    sourceFileTextCount: Int,
    localSourceText: UnsafePointer<UInt8>,
    localSourceTextCount: Int
  ) -> (code: UnsafePointer<UInt8>?, codeLength: Int,
        diagnostics: UnsafePointer<_Diagnostic>?,
        diagnosticCount: Int) {
    let meeTextBuffer = UnsafeBufferPointer(
      start: localSourceText, count: localSourceTextCount)
    let meeText = String(decoding: meeTextBuffer, as: UTF8.self)
    let prefix = "#customStringify("
    guard meeText.starts(with: prefix), meeText.last == ")" else {
      return (nil, 0, nil, 0)
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

    let exprText = "\(expr)"
    var resultString = """
    (\(exprText),
    #\"\"\"
    \(exprText)
    \"\"\"#)
    """
    let (resultBuffer, resultLength) = resultString.withUTF8 { buffer in
      let result = UnsafeMutableBufferPointer<UInt8>.allocate(
        capacity: buffer.count + 1)
      _ = result.initialize(from: buffer)
      result[buffer.count] = 0
      return (UnsafePointer(result.baseAddress), buffer.count)
    }

    let localSourceOffset = sourceFileText.distance(to: localSourceText)
    let diags = UnsafeMutablePointer<_Diagnostic>.allocate(capacity: 2)
    diags[0] = _makeDiagnostic(
      message: "test note", position: localSourceOffset, severity: .note)
    diags[1] = _makeDiagnostic(
      message: "test warning",
      position: localSourceOffset + localSourceTextCount - 1,
      severity: .warning)

    return (
      code: resultBuffer, codeLength: resultLength,
      diagnostics: UnsafePointer?(diags), diagnosticCount: 2
    )
  }
}

public struct ColorLiteralMacro: _CompilerPlugin {
  public static func _kind() -> _CompilerPluginKind {
    .expressionMacro
  }

  public static func _rewrite(
    targetModuleName: UnsafePointer<UInt8>,
    targetModuleNameCount: Int,
    filePath: UnsafePointer<UInt8>,
    filePathCount: Int,
    sourceFileText: UnsafePointer<UInt8>,
    sourceFileTextCount: Int,
    localSourceText: UnsafePointer<UInt8>,
    localSourceTextCount: Int
  ) -> (code: UnsafePointer<UInt8>?, codeLength: Int,
        diagnostics: UnsafePointer<_Diagnostic>?,
        diagnosticCount: Int) {
    let meeTextBuffer = UnsafeBufferPointer(
      start: localSourceText, count: localSourceTextCount)
    let meeText = String(decoding: meeTextBuffer, as: UTF8.self)
    let prefix = "#customColorLiteral(red:"
    guard meeText.starts(with: prefix), meeText.last == ")" else {
      return (nil, 0, nil, 0)
    }
    let expr = meeText.dropFirst(prefix.count).dropLast()
    var resultString = ".init(_colorLiteralRed:\(expr))"
    return resultString.withUTF8 { buffer in
      let result = UnsafeMutableBufferPointer<UInt8>.allocate(
          capacity: buffer.count + 1)
      _ = result.initialize(from: buffer)
      result[buffer.count] = 0
      return (UnsafePointer(result.baseAddress), buffer.count, nil, 0)
    }
  }
}

public struct HSVColorLiteralMacro: _CompilerPlugin {
  public static func _kind() -> _CompilerPluginKind {
    .expressionMacro
  }

  public static func _rewrite(
    targetModuleName: UnsafePointer<UInt8>,
    targetModuleNameCount: Int,
    filePath: UnsafePointer<UInt8>,
    filePathCount: Int,
    sourceFileText: UnsafePointer<UInt8>,
    sourceFileTextCount: Int,
    localSourceText: UnsafePointer<UInt8>,
    localSourceTextCount: Int
  ) -> (code: UnsafePointer<UInt8>?, codeLength: Int,
        diagnostics: UnsafePointer<_Diagnostic>?,
        diagnosticCount: Int) {
    let meeTextBuffer = UnsafeBufferPointer(
      start: localSourceText, count: localSourceTextCount)
    let meeText = String(decoding: meeTextBuffer, as: UTF8.self)
    let prefix = "#customColorLiteral(hue:"
    guard meeText.starts(with: prefix), meeText.last == ")" else {
      return (nil, 0, nil, 0)
    }
    let expr = meeText.dropFirst(prefix.count).dropLast()
    var resultString = ".init(_colorLiteralHue:\(expr))"
    return resultString.withUTF8 { buffer in
      let result = UnsafeMutableBufferPointer<UInt8>.allocate(
          capacity: buffer.count + 1)
      _ = result.initialize(from: buffer)
      result[buffer.count] = 0
      return (UnsafePointer(result.baseAddress), buffer.count, nil, 0)
    }
  }
}
