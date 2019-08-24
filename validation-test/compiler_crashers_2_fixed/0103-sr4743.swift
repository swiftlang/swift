// RUN: not %target-swift-frontend -typecheck -primary-file %s

public struct _UIntBuffer<Storage, Element> {
  var _storage: Storage
  var _bitCount: UInt8
}

protocol _UTFDecoder {
  associatedtype BufferStorage
  associatedtype CodeUnit

  var buffer: _UIntBuffer<BufferStorage, CodeUnit> { get set }
}

public struct ReverseDecoder : _UTFDecoder {
  public typealias Buffer = _UIntBuffer<BufferStorage, UInt8>
  public var buffer = Buffer()
  public init() {}
}
