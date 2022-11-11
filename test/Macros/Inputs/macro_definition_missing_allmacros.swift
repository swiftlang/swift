import _CompilerPluginSupport
import RegexBuilder

struct DummyMacro: _CompilerPlugin {
  static func _name() -> (UnsafePointer<UInt8>, count: Int) {
    var name = "dummy"
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
    (nil, 0)
  }
}

// Intentionally missing `allMacros`.
//
// public var allMacros: [Any.Type] {
//   [DummyMacro.self]
// }
