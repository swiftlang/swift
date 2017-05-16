//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

@_transparent
internal func _abstract(
  methodName: StaticString = #function,
  file: StaticString = #file, line: UInt = #line
) -> Never {
#if INTERNAL_CHECKS_ENABLED
  _fatalErrorMessage("abstract method", methodName, file: file, line: line,
      flags: _fatalErrorFlags())
#else
  _conditionallyUnreachable()
#endif
}

// MARK: Type-erased abstract base classes

/// A type-erased key path, from any root type to any resulting value type.
public class AnyKeyPath: Hashable, _AppendKeyPath {
  /// The root type for this key path.
  @_inlineable
  public static var rootType: Any.Type {
    return _rootAndValueType.root
  }

  /// The value type for this key path.
  @_inlineable
  public static var valueType: Any.Type {
    return _rootAndValueType.value
  }

  internal final var _kvcKeyPathStringPtr: UnsafePointer<CChar>?
  
  final public var hashValue: Int {
    var hash = 0
    withBuffer {
      var buffer = $0
      while true {
        let (component, type) = buffer.next()
        hash ^= _mixInt(component.value.hashValue)
        if let type = type {
          hash ^= _mixInt(unsafeBitCast(type, to: Int.self))
        } else {
          break
        }
      }
    }
    return hash
  }
  public static func ==(a: AnyKeyPath, b: AnyKeyPath) -> Bool {
    // Fast-path identical objects
    if a === b {
      return true
    }
    // Short-circuit differently-typed key paths
    if type(of: a) != type(of: b) {
      return false
    }
    return a.withBuffer {
      var aBuffer = $0
      return b.withBuffer {
        var bBuffer = $0
        
        // Two equivalent key paths should have the same reference prefix
        if aBuffer.hasReferencePrefix != bBuffer.hasReferencePrefix {
          return false
        }
        
        while true {
          let (aComponent, aType) = aBuffer.next()
          let (bComponent, bType) = bBuffer.next()
        
          if aComponent.header.endOfReferencePrefix
              != bComponent.header.endOfReferencePrefix
            || aComponent.value != bComponent.value
            || aType != bType {
            return false
          }
          if aType == nil {
            return true
          }
        }
      }
    }
  }

  // SPI for the Foundation overlay to allow interop with KVC keypath-based
  // APIs.
  public var _kvcKeyPathString: String? {
    guard let ptr = _kvcKeyPathStringPtr else { return nil }

    return String(validatingUTF8: ptr)
  }
  
  // MARK: Implementation details
  
  // Prevent normal initialization. We use tail allocation via
  // allocWithTailElems().
  internal init() {
    _sanityCheckFailure("use _create(...)")
  }
  
  // internal-with-availability
  public class var _rootAndValueType: (root: Any.Type, value: Any.Type) {
    _abstract()
  }
  
  public // @testable
  static func _create(
    capacityInBytes bytes: Int,
    initializedBy body: (UnsafeMutableRawBufferPointer) -> Void
  ) -> Self {
    _sanityCheck(bytes > 0 && bytes % 4 == 0,
                 "capacity must be multiple of 4 bytes")
    let result = Builtin.allocWithTailElems_1(self, (bytes/4)._builtinWordValue,
                                              Int32.self)
    result._kvcKeyPathStringPtr = nil
    let base = UnsafeMutableRawPointer(Builtin.projectTailElems(result,
                                                                Int32.self))
    body(UnsafeMutableRawBufferPointer(start: base, count: bytes))
    return result
  }
  
  func withBuffer<T>(_ f: (KeyPathBuffer) throws -> T) rethrows -> T {
    defer { _fixLifetime(self) }
    
    let base = UnsafeRawPointer(Builtin.projectTailElems(self, Int32.self))
    return try f(KeyPathBuffer(base: base))
  }
}

/// A partially type-erased key path, from a concrete root type to any
/// resulting value type.
public class PartialKeyPath<Root>: AnyKeyPath { }

// MARK: Concrete implementations
internal enum KeyPathKind { case readOnly, value, reference }

/// A key path from a specific root type to a specific resulting value type.
public class KeyPath<Root, Value>: PartialKeyPath<Root> {
  public typealias _Root = Root
  public typealias _Value = Value

  public final override class var _rootAndValueType: (
    root: Any.Type,
    value: Any.Type
  ) {
    return (Root.self, Value.self)
  }
  
  // MARK: Implementation
  typealias Kind = KeyPathKind
  class var kind: Kind { return .readOnly }
  
  static func appendedType<AppendedValue>(
    with t: KeyPath<Value, AppendedValue>.Type
  ) -> KeyPath<Root, AppendedValue>.Type {
    let resultKind: Kind
    switch (self.kind, t.kind) {
    case (_, .reference):
      resultKind = .reference
    case (let x, .value):
      resultKind = x
    default:
      resultKind = .readOnly
    }
    
    switch resultKind {
    case .readOnly:
      return KeyPath<Root, AppendedValue>.self
    case .value:
      return WritableKeyPath.self
    case .reference:
      return ReferenceWritableKeyPath.self
    }
  }
  
  final func projectReadOnly(from root: Root) -> Value {
    // TODO: For perf, we could use a local growable buffer instead of Any
    var curBase: Any = root
    return withBuffer {
      var buffer = $0
      while true {
        let (rawComponent, optNextType) = buffer.next()
        let valueType = optNextType ?? Value.self
        let isLast = optNextType == nil
        
        func project<CurValue>(_ base: CurValue) -> Value? {
          func project2<NewValue>(_: NewValue.Type) -> Value? {
            let newBase: NewValue = rawComponent.projectReadOnly(base)
            if isLast {
              _sanityCheck(NewValue.self == Value.self,
                           "key path does not terminate in correct type")
              return unsafeBitCast(newBase, to: Value.self)
            } else {
              curBase = newBase
              return nil
            }
          }

          return _openExistential(valueType, do: project2)
        }

        if let result = _openExistential(curBase, do: project) {
          return result
        }
      }
    }
  }
  
  deinit {
    withBuffer { $0.destroy() }
  }
}

/// A key path that supports reading from and writing to the resulting value.
public class WritableKeyPath<Root, Value>: KeyPath<Root, Value> {
  // MARK: Implementation detail
  
  override class var kind: Kind { return .value }

  // `base` is assumed to be undergoing a formal access for the duration of the
  // call, so must not be mutated by an alias
  func projectMutableAddress(from base: UnsafePointer<Root>)
      -> (pointer: UnsafeMutablePointer<Value>, owner: Builtin.NativeObject) {
    var p = UnsafeRawPointer(base)
    var type: Any.Type = Root.self
    var keepAlive: [AnyObject] = []
    
    return withBuffer {
      var buffer = $0
      
      _sanityCheck(!buffer.hasReferencePrefix,
                   "WritableKeyPath should not have a reference prefix")
      
      while true {
        let (rawComponent, optNextType) = buffer.next()
        let nextType = optNextType ?? Value.self
        
        func project<CurValue>(_: CurValue.Type) {
          func project2<NewValue>(_: NewValue.Type) {
            p = rawComponent.projectMutableAddress(p,
                                           from: CurValue.self,
                                           to: NewValue.self,
                                           isRoot: p == UnsafeRawPointer(base),
                                           keepAlive: &keepAlive)
          }
          _openExistential(nextType, do: project2)
        }
        _openExistential(type, do: project)
        
        if optNextType == nil { break }
        type = nextType
      }
      // TODO: With coroutines, it would be better to yield here, so that
      // we don't need the hack of the keepAlive array to manage closing
      // accesses.
      let typedPointer = p.assumingMemoryBound(to: Value.self)
      return (pointer: UnsafeMutablePointer(mutating: typedPointer),
              owner: keepAlive._getOwner_native())
    }
  }

}

/// A key path that supports reading from and writing to the resulting value
/// with reference semantics.
public class ReferenceWritableKeyPath<Root, Value>: WritableKeyPath<Root, Value> {
  // MARK: Implementation detail

  final override class var kind: Kind { return .reference }
  
  final override func projectMutableAddress(from base: UnsafePointer<Root>)
      -> (pointer: UnsafeMutablePointer<Value>, owner: Builtin.NativeObject) {
    // Since we're a ReferenceWritableKeyPath, we know we don't mutate the base in
    // practice.
    return projectMutableAddress(from: base.pointee)
  }
  
  final func projectMutableAddress(from origBase: Root)
      -> (pointer: UnsafeMutablePointer<Value>, owner: Builtin.NativeObject) {
    var keepAlive: [AnyObject] = []
    var address: UnsafeMutablePointer<Value> = withBuffer {
      var buffer = $0
      // Project out the reference prefix.
      var base: Any = origBase
      while buffer.hasReferencePrefix {
        let (rawComponent, optNextType) = buffer.next()
        _sanityCheck(optNextType != nil,
                     "reference prefix should not go to end of buffer")
        let nextType = optNextType.unsafelyUnwrapped
        
        func project<NewValue>(_: NewValue.Type) -> Any {
          func project2<CurValue>(_ base: CurValue) -> Any {
            return rawComponent.projectReadOnly(base) as NewValue
          }
          return _openExistential(base, do: project2)
        }
        base = _openExistential(nextType, do: project)
      }
      
      // Start formal access to the mutable value, based on the final base
      // value.
      func formalMutation<MutationRoot>(_ base: MutationRoot)
          -> UnsafeMutablePointer<Value> {
        var base2 = base
        return withUnsafeBytes(of: &base2) { baseBytes in
          var p = baseBytes.baseAddress.unsafelyUnwrapped
          var curType: Any.Type = MutationRoot.self
          while true {
            let (rawComponent, optNextType) = buffer.next()
            let nextType = optNextType ?? Value.self
            func project<CurValue>(_: CurValue.Type) {
              func project2<NewValue>(_: NewValue.Type) {
                p = rawComponent.projectMutableAddress(p,
                                             from: CurValue.self,
                                             to: NewValue.self,
                                             isRoot: p == baseBytes.baseAddress,
                                             keepAlive: &keepAlive)
              }
              _openExistential(nextType, do: project2)
            }
            _openExistential(curType, do: project)

            if optNextType == nil { break }
            curType = nextType
          }
          let typedPointer = p.assumingMemoryBound(to: Value.self)
          return UnsafeMutablePointer(mutating: typedPointer)
        }
      }
      return _openExistential(base, do: formalMutation)
    }
    
    return (address, keepAlive._getOwner_native())
  }
}

// MARK: Implementation details

// Keypaths store word-sized values with 32-bit alignment for memory efficiency.
// Since RawPointer's APIs currently require alignment, this means we need
// to do some shuffling for the unaligned load/stores.

extension UnsafeRawBufferPointer {
  internal func _loadKeyPathWord<T>(fromByteOffset offset: Int = 0,
                                    as _: T.Type) -> T {
    _sanityCheck(_isPOD(T.self) &&
                 MemoryLayout<T>.size == MemoryLayout<Int>.size,
                 "not a word-sized trivial type")
    if MemoryLayout<Int>.size == 8 {
      let words = load(fromByteOffset: offset, as: (Int32, Int32).self)
      return unsafeBitCast(words, to: T.self)
    } else if MemoryLayout<Int>.size == 4 {
      return load(fromByteOffset: offset, as: T.self)
    } else {
      _sanityCheckFailure("unsupported architecture")
    }
  }
}

extension UnsafeMutableRawBufferPointer {
  internal func _loadKeyPathWord<T>(fromByteOffset offset: Int = 0,
                                    as _: T.Type) -> T {
    _sanityCheck(_isPOD(T.self) &&
                 MemoryLayout<T>.size == MemoryLayout<Int>.size,
                 "not a word-sized trivial type")
    if MemoryLayout<Int>.size == 8 {
      let words = load(fromByteOffset: offset, as: (Int32, Int32).self)
      return unsafeBitCast(words, to: T.self)
    } else if MemoryLayout<Int>.size == 4 {
      return load(fromByteOffset: offset, as: T.self)
    } else {
      _sanityCheckFailure("unsupported architecture")
    }
  }
  internal func _storeKeyPathWord<T>(of value: T,
                                     toByteOffset offset: Int = 0) {
    _sanityCheck(_isPOD(T.self) &&
                 MemoryLayout<T>.size == MemoryLayout<Int>.size,
                 "not a word-sized trivial type")
    if MemoryLayout<Int>.size == 8 {
      let words = unsafeBitCast(value, to: (Int32, Int32).self)
      storeBytes(of: words, toByteOffset: offset, as: (Int32,Int32).self)
    } else if MemoryLayout<Int>.size == 4 {
      storeBytes(of: value, toByteOffset: offset, as: T.self)
    } else {
      _sanityCheckFailure("unsupported architecture")
    }
  }
}

internal enum KeyPathComponentKind {
  /// The keypath projects within the storage of the outer value, like a
  /// stored property in a struct.
  case `struct`
  /// The keypath projects from the referenced pointer, like a
  /// stored property in a class.
  case `class`
  /// The keypath projects using a getter/setter pair.
  case computed
  /// The keypath optional-chains, returning nil immediately if the input is
  /// nil, or else proceeding by projecting the value inside.
  case optionalChain
  /// The keypath optional-forces, trapping if the input is
  /// nil, or else proceeding by projecting the value inside.
  case optionalForce
  /// The keypath wraps a value in an optional.
  case optionalWrap
}

internal struct ComputedPropertyID: Hashable {
  var value: Int
  var isStoredProperty: Bool
  var isTableOffset: Bool

  static func ==(x: ComputedPropertyID, y: ComputedPropertyID) -> Bool {
    return x.value == y.value
      && x.isStoredProperty == y.isStoredProperty
      && x.isTableOffset == x.isTableOffset
  }

  var hashValue: Int {
    var hash = 0
    hash ^= _mixInt(value)
    hash ^= _mixInt(isStoredProperty ? 13 : 17)
    hash ^= _mixInt(isTableOffset ? 19 : 23)
    return hash
  }
}

internal enum KeyPathComponent: Hashable {
  /// The keypath projects within the storage of the outer value, like a
  /// stored property in a struct.
  case `struct`(offset: Int)
  /// The keypath projects from the referenced pointer, like a
  /// stored property in a class.
  case `class`(offset: Int)
  /// The keypath projects using a getter.
  case get(id: ComputedPropertyID,
           get: UnsafeRawPointer, argument: UnsafeRawPointer)
  /// The keypath projects using a getter/setter pair. The setter can mutate
  /// the base value in-place.
  case mutatingGetSet(id: ComputedPropertyID,
                      get: UnsafeRawPointer, set: UnsafeRawPointer,
                      argument: UnsafeRawPointer)
  /// The keypath projects using a getter/setter pair that does not mutate its
  /// base.
  case nonmutatingGetSet(id: ComputedPropertyID,
                         get: UnsafeRawPointer, set: UnsafeRawPointer,
                         argument: UnsafeRawPointer)
  /// The keypath optional-chains, returning nil immediately if the input is
  /// nil, or else proceeding by projecting the value inside.
  case optionalChain
  /// The keypath optional-forces, trapping if the input is
  /// nil, or else proceeding by projecting the value inside.
  case optionalForce
  /// The keypath wraps a value in an optional.
  case optionalWrap

  static func ==(a: KeyPathComponent, b: KeyPathComponent) -> Bool {
    switch (a, b) {
    case (.struct(offset: let a), .struct(offset: let b)),
         (.class (offset: let a), .class (offset: let b)):
      return a == b
    case (.optionalChain, .optionalChain),
         (.optionalForce, .optionalForce),
         (.optionalWrap, .optionalWrap):
      return true
    case (.get(id: let id1, get: _, argument: _),
          .get(id: let id2, get: _, argument: _)):
      return id1 == id2
    case (.mutatingGetSet(id: let id1, get: _, set: _, argument: _),
          .mutatingGetSet(id: let id2, get: _, set: _, argument: _)):
      return id1 == id2
    case (.nonmutatingGetSet(id: let id1, get: _, set: _, argument: _),
          .nonmutatingGetSet(id: let id2, get: _, set: _, argument: _)):
      return id1 == id2
    case (.struct, _),
         (.class,  _),
         (.optionalChain, _),
         (.optionalForce, _),
         (.optionalWrap, _),
         (.get, _),
         (.mutatingGetSet, _),
         (.nonmutatingGetSet, _):
      return false
    }
  }
  
  var hashValue: Int {
    var hash: Int = 0
    switch self {
    case .struct(offset: let a):
      hash ^= _mixInt(0)
      hash ^= _mixInt(a)
    case .class(offset: let b):
      hash ^= _mixInt(1)
      hash ^= _mixInt(b)
    case .optionalChain:
      hash ^= _mixInt(2)
    case .optionalForce:
      hash ^= _mixInt(3)
    case .optionalWrap:
      hash ^= _mixInt(4)
    case .get(id: let id, get: _, argument: _):
      hash ^= _mixInt(5)
      hash ^= _mixInt(id.hashValue)
    case .mutatingGetSet(id: let id, get: _, set: _, argument: _):
      hash ^= _mixInt(6)
      hash ^= _mixInt(id.hashValue)
    case .nonmutatingGetSet(id: let id, get: _, set: _, argument: _):
      hash ^= _mixInt(7)
      hash ^= _mixInt(id.hashValue)
    }
    return hash
  }
}

// A class that triggers writeback to a pointer when destroyed.
internal final class MutatingWritebackBuffer<CurValue, NewValue> {
  let base: UnsafeMutablePointer<CurValue>
  let set: @convention(thin) (NewValue, inout CurValue, UnsafeRawPointer) -> ()
  let argument: UnsafeRawPointer
  var value: NewValue

  deinit {
    set(value, &base.pointee, argument)
  }

  init(base: UnsafeMutablePointer<CurValue>,
       set: @escaping @convention(thin) (NewValue, inout CurValue, UnsafeRawPointer) -> (),
       argument: UnsafeRawPointer,
       value: NewValue) {
    self.base = base
    self.set = set
    self.argument = argument
    self.value = value
  }
}

// A class that triggers writeback to a non-mutated value when destroyed.
internal final class NonmutatingWritebackBuffer<CurValue, NewValue> {
  let base: CurValue
  let set: @convention(thin) (NewValue, CurValue, UnsafeRawPointer) -> ()
  let argument: UnsafeRawPointer
  var value: NewValue

  deinit {
    set(value, base, argument)
  }

  init(base: CurValue,
       set: @escaping @convention(thin) (NewValue, CurValue, UnsafeRawPointer) -> (),
       argument: UnsafeRawPointer,
       value: NewValue) {
    self.base = base
    self.set = set
    self.argument = argument
    self.value = value
  }
}

internal struct RawKeyPathComponent {
  var header: Header
  var body: UnsafeRawBufferPointer
  
  struct Header {
    static var payloadMask: UInt32 {
      return _SwiftKeyPathComponentHeader_PayloadMask
    }
    static var discriminatorMask: UInt32 {
      return _SwiftKeyPathComponentHeader_DiscriminatorMask
    }
    static var discriminatorShift: UInt32 {
      return _SwiftKeyPathComponentHeader_DiscriminatorShift
    }
    static var structTag: UInt32 {
      return _SwiftKeyPathComponentHeader_StructTag
    }
    static var computedTag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedTag
    }
    static var classTag: UInt32 {
      return _SwiftKeyPathComponentHeader_ClassTag
    }
    static var optionalTag: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalTag
    }
    static var optionalChainPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalChainPayload
    }
    static var optionalWrapPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalWrapPayload
    }
    static var optionalForcePayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalForcePayload
    }
    static var endOfReferencePrefixFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_EndOfReferencePrefixFlag
    }
    static var outOfLineOffsetPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OutOfLineOffsetPayload
    }
    static var unresolvedOffsetPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_UnresolvedOffsetPayload
    }
    static var computedMutatingFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedMutatingFlag
    }
    static var computedSettableFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedSettableFlag
    }
    static var computedIDByStoredPropertyFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDByStoredPropertyFlag
    }
    static var computedIDByVTableOffsetFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDByVTableOffsetFlag
    }
    static var computedHasArgumentsFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedHasArgumentsFlag
    }

    static var computedIDResolutionMask: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDResolutionMask
    }
    static var computedIDResolved: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDResolved
    }
    static var computedIDUnresolvedFieldOffset: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDUnresolvedFieldOffset
    }
    static var computedIDUnresolvedIndirectPointer: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDUnresolvedIndirectPointer
    }
    
    var _value: UInt32
    
    var discriminator: UInt32 {
      return (_value & Header.discriminatorMask) >> Header.discriminatorShift
    }
    var payload: UInt32 {
      get {
        return _value & Header.payloadMask
      }
      set {
        _sanityCheck(newValue & Header.payloadMask == newValue,
                     "payload too big")
        _value = _value & ~Header.payloadMask | newValue
      }
    }
    var endOfReferencePrefix: Bool {
      get {
        return _value & Header.endOfReferencePrefixFlag != 0
      }
      set {
        if newValue {
          _value |= Header.endOfReferencePrefixFlag
        } else {
          _value &= ~Header.endOfReferencePrefixFlag
        }
      }
    }

    var kind: KeyPathComponentKind {
      switch (discriminator, payload) {
      case (Header.structTag, _):
        return .struct
      case (Header.classTag, _):
        return .class
      case (Header.computedTag, _):
        return .computed
      case (Header.optionalTag, Header.optionalChainPayload):
        return .optionalChain
      case (Header.optionalTag, Header.optionalWrapPayload):
        return .optionalWrap
      case (Header.optionalTag, Header.optionalForcePayload):
        return .optionalForce
      default:
        _sanityCheckFailure("invalid header")
      }
    }
    
    var bodySize: Int {
      switch kind {
      case .struct, .class:
        if payload == Header.payloadMask { return 4 } // overflowed
        return 0
      case .optionalChain, .optionalForce, .optionalWrap:
        return 0
      case .computed:
        let ptrSize = MemoryLayout<Int>.size
        // minimum two pointers for id and get
        var total = ptrSize * 2
        // additional word for a setter
        if payload & Header.computedSettableFlag != 0 {
          total += ptrSize
        }
        // TODO: Include the argument size
        _sanityCheck(payload & Header.computedHasArgumentsFlag == 0,
                     "arguments not implemented")
        return total
      }
    }
    
    var isTrivial: Bool {
      switch kind {
      case .struct, .class, .optionalChain, .optionalForce, .optionalWrap:
        return true
      case .computed:
        // TODO: consider nontrivial arguments
        _sanityCheck(payload & Header.computedHasArgumentsFlag == 0,
                     "arguments not implemented")
        return true
      }
    }
  }

  var _structOrClassOffset: Int {
    _sanityCheck(header.kind == .struct || header.kind == .class,
                 "no offset for this kind")
    // An offset too large to fit inline is represented by a signal and stored
    // in the body.
    if header.payload == Header.outOfLineOffsetPayload {
      // Offset overflowed into body
      _sanityCheck(body.count >= MemoryLayout<UInt32>.size,
                   "component not big enough")
      return Int(body.load(as: UInt32.self))
    }
    return Int(header.payload)
  }

  var _computedIDValue: Int {
    _sanityCheck(header.kind == .computed,
                 "not a computed property")
    _sanityCheck(body.count >= MemoryLayout<Int>.size,
                 "component is not big enough")
    return body._loadKeyPathWord(as: Int.self)
  }

  var _computedID: ComputedPropertyID {
    let payload = header.payload
    return ComputedPropertyID(
      value: _computedIDValue,
      isStoredProperty: payload & Header.computedIDByStoredPropertyFlag != 0,
      isTableOffset: payload & Header.computedIDByVTableOffsetFlag != 0)
  }

  var _computedGetter: UnsafeRawPointer {
    _sanityCheck(header.kind == .computed,
                 "not a computed property")
    _sanityCheck(body.count >= MemoryLayout<Int>.size * 2,
                 "component is not big enough")

    return body._loadKeyPathWord(fromByteOffset: MemoryLayout<Int>.size,
                                 as: UnsafeRawPointer.self)
  }

  var _computedSetter: UnsafeRawPointer {
    _sanityCheck(header.kind == .computed,
                 "not a computed property")
    _sanityCheck(header.payload & Header.computedSettableFlag != 0,
                 "not a settable property")
    _sanityCheck(body.count >= MemoryLayout<Int>.size * 3,
                 "component is not big enough")

    return body._loadKeyPathWord(fromByteOffset: MemoryLayout<Int>.size * 2,
                                 as: UnsafeRawPointer.self)
  }

  var value: KeyPathComponent {
    switch header.kind {
    case .struct:
      return .struct(offset: _structOrClassOffset)
    case .class:
      return .class(offset: _structOrClassOffset)
    case .optionalChain:
      return .optionalChain
    case .optionalForce:
      return .optionalForce
    case .optionalWrap:
      return .optionalWrap
    case .computed:
      let isSettable = header.payload & Header.computedSettableFlag != 0
      let isMutating = header.payload & Header.computedMutatingFlag != 0
      _sanityCheck(header.payload & Header.computedHasArgumentsFlag == 0,
                   "arguments not implemented")

      let id = _computedID
      let get = _computedGetter
      switch (isSettable, isMutating) {
      case (false, false):
        return .get(id: id, get: get, argument: get)
      case (true, false):
        return .nonmutatingGetSet(id: id,
                                  get: get,
                                  set: _computedSetter,
                                  argument: get)
      case (true, true):
        return .mutatingGetSet(id: id,
                               get: get,
                               set: _computedSetter,
                               argument: get)
      case (false, true):
        _sanityCheckFailure("impossible")
      }
    }
  }

  func destroy() {
    switch header.kind {
    case .struct,
         .class,
         .optionalChain,
         .optionalForce,
         .optionalWrap:
      // trivial
      return
    case .computed:
      // TODO: consider nontrivial arguments
      _sanityCheck(header.payload & Header.computedHasArgumentsFlag == 0,
                   "arguments not implemented")
      return
    }
  }
  
  func clone(into buffer: inout UnsafeMutableRawBufferPointer,
             endOfReferencePrefix: Bool) {
    var newHeader = header
    newHeader.endOfReferencePrefix = endOfReferencePrefix

    var componentSize = MemoryLayout<Header>.size
    buffer.storeBytes(of: newHeader, as: Header.self)
    switch header.kind {
    case .struct,
         .class:
      if header.payload == Header.outOfLineOffsetPayload {
        let overflowOffset = body.load(as: UInt32.self)
        buffer.storeBytes(of: overflowOffset, toByteOffset: 4,
                          as: UInt32.self)
        componentSize += 4
      }
    case .optionalChain,
         .optionalForce,
         .optionalWrap:
      break
    case .computed:
      // TODO: nontrivial arguments need to be copied by value witness
      _sanityCheck(header.payload & Header.computedHasArgumentsFlag == 0,
                   "arguments not implemented")
      buffer._storeKeyPathWord(of: _computedIDValue, toByteOffset: 4)
      buffer._storeKeyPathWord(of: _computedGetter,
                               toByteOffset: 4 + MemoryLayout<Int>.size)

      componentSize += MemoryLayout<Int>.size * 2

      if header.payload & Header.computedSettableFlag != 0 {
        buffer._storeKeyPathWord(of: _computedSetter,
                                 toByteOffset: 4 + MemoryLayout<Int>.size * 2)
        componentSize += MemoryLayout<Int>.size
      }
    }
    _sanityCheck(buffer.count >= componentSize)
    buffer = UnsafeMutableRawBufferPointer(
      start: buffer.baseAddress.unsafelyUnwrapped + componentSize,
      count: buffer.count - componentSize
    )
  }
  
  func projectReadOnly<CurValue, NewValue>(_ base: CurValue) -> NewValue {
    switch value {
    case .struct(let offset):
      var base2 = base
      return withUnsafeBytes(of: &base2) {
        let p = $0.baseAddress.unsafelyUnwrapped.advanced(by: offset)
        // The contents of the struct should be well-typed, so we can assume
        // typed memory here.
        return p.assumingMemoryBound(to: NewValue.self).pointee
      }
    
    case .class(let offset):
      _sanityCheck(CurValue.self is AnyObject.Type,
                   "base is not a class")
      let baseObj = unsafeBitCast(base, to: AnyObject.self)
      let basePtr = UnsafeRawPointer(Builtin.bridgeToRawPointer(baseObj))
      defer { _fixLifetime(baseObj) }
      return basePtr.advanced(by: offset)
        .assumingMemoryBound(to: NewValue.self)
        .pointee
    
    case .get(id: _, get: let rawGet, argument: let argument),
         .mutatingGetSet(id: _, get: let rawGet, set: _, argument: let argument),
         .nonmutatingGetSet(id: _, get: let rawGet, set: _, argument: let argument):
      typealias Getter
        = @convention(thin) (CurValue, UnsafeRawPointer) -> NewValue
      let get = unsafeBitCast(rawGet, to: Getter.self)
      return get(base, argument)

    case .optionalChain:
      fatalError("TODO")
    
    case .optionalForce:
      fatalError("TODO")
      
    case .optionalWrap:
      fatalError("TODO")
    }
  }
  
  func projectMutableAddress<CurValue, NewValue>(
    _ base: UnsafeRawPointer,
    from _: CurValue.Type,
    to _: NewValue.Type,
    isRoot: Bool,
    keepAlive: inout [AnyObject]
  ) -> UnsafeRawPointer {
    switch value {
    case .struct(let offset):
      return base.advanced(by: offset)
    case .class(let offset):
      // A class dereference should only occur at the root of a mutation,
      // since otherwise it would be part of the reference prefix.
      _sanityCheck(isRoot,
                 "class component should not appear in the middle of mutation")
      // AnyObject memory can alias any class reference memory, so we can
      // assume type here
      let object = base.assumingMemoryBound(to: AnyObject.self).pointee
      // The base ought to be kept alive for the duration of the derived access
      keepAlive.append(object)
      return UnsafeRawPointer(Builtin.bridgeToRawPointer(object))
            .advanced(by: offset)
    
    case .mutatingGetSet(id: _, get: let rawGet, set: let rawSet,
                         argument: let argument):
      typealias Getter
        = @convention(thin) (CurValue, UnsafeRawPointer) -> NewValue
      typealias Setter
        = @convention(thin) (NewValue, inout CurValue, UnsafeRawPointer) -> ()
      let get = unsafeBitCast(rawGet, to: Getter.self)
      let set = unsafeBitCast(rawSet, to: Setter.self)

      let baseTyped = UnsafeMutablePointer(
        mutating: base.assumingMemoryBound(to: CurValue.self))

      let writeback = MutatingWritebackBuffer(base: baseTyped,
                                       set: set,
                                       argument: argument,
                                       value: get(baseTyped.pointee, argument))
      keepAlive.append(writeback)
      // A maximally-abstracted, final, stored class property should have
      // a stable address.
      return UnsafeRawPointer(Builtin.addressof(&writeback.value))

    case .nonmutatingGetSet(id: _, get: let rawGet, set: let rawSet,
                            argument: let argument):
      // A nonmutating property should only occur at the root of a mutation,
      // since otherwise it would be part of the reference prefix.
      _sanityCheck(isRoot,
           "nonmutating component should not appear in the middle of mutation")

      typealias Getter
        = @convention(thin) (CurValue, UnsafeRawPointer) -> NewValue
      typealias Setter
        = @convention(thin) (NewValue, CurValue, UnsafeRawPointer) -> ()

      let get = unsafeBitCast(rawGet, to: Getter.self)
      let set = unsafeBitCast(rawSet, to: Setter.self)

      let baseValue = base.assumingMemoryBound(to: CurValue.self).pointee
      let writeback = NonmutatingWritebackBuffer(base: baseValue,
                                               set: set,
                                               argument: argument,
                                               value: get(baseValue, argument))
      keepAlive.append(writeback)
      // A maximally-abstracted, final, stored class property should have
      // a stable address.
      return UnsafeRawPointer(Builtin.addressof(&writeback.value))

    case .optionalForce:
      fatalError("TODO")
    
    case .optionalChain, .optionalWrap, .get:
      _sanityCheckFailure("not a mutable key path component")
    }
  }
}

internal struct KeyPathBuffer {
  var data: UnsafeRawBufferPointer
  var trivial: Bool
  var hasReferencePrefix: Bool

  var mutableData: UnsafeMutableRawBufferPointer {
    return UnsafeMutableRawBufferPointer(mutating: data)
  }

  struct Header {
    var _value: UInt32
    
    static var sizeMask: UInt32 {
      return _SwiftKeyPathBufferHeader_SizeMask
    }
    static var reservedMask: UInt32 {
      return _SwiftKeyPathBufferHeader_ReservedMask
    }
    static var trivialFlag: UInt32 {
      return _SwiftKeyPathBufferHeader_TrivialFlag
    }
    static var hasReferencePrefixFlag: UInt32 {
      return _SwiftKeyPathBufferHeader_HasReferencePrefixFlag
    }

    init(size: Int, trivial: Bool, hasReferencePrefix: Bool) {
      _sanityCheck(size <= Int(Header.sizeMask), "key path too big")
      _value = UInt32(size)
        | (trivial ? Header.trivialFlag : 0)
        | (hasReferencePrefix ? Header.hasReferencePrefixFlag : 0)
    }

    var size: Int { return Int(_value & Header.sizeMask) }
    var trivial: Bool { return _value & Header.trivialFlag != 0 }
    var hasReferencePrefix: Bool {
      get {
        return _value & Header.hasReferencePrefixFlag != 0
      }
      set {
        if newValue {
          _value |= Header.hasReferencePrefixFlag
        } else {
          _value &= ~Header.hasReferencePrefixFlag
        }
      }
    }

    // In a key path pattern, the "trivial" flag is used to indicate
    // "instantiable in-line"
    var instantiableInLine: Bool {
      return trivial
    }

    func validateReservedBits() {
      _precondition(_value & Header.reservedMask == 0,
                    "reserved bits set to an unexpected bit pattern")
    }
  }

  init(base: UnsafeRawPointer) {
    let header = base.load(as: Header.self)
    data = UnsafeRawBufferPointer(
      start: base + MemoryLayout<Header>.size,
      count: header.size
    )
    trivial = header.trivial
    hasReferencePrefix = header.hasReferencePrefix
  }
  
  func destroy() {
    if trivial { return }
    fatalError("TODO")
  }
  
  mutating func next() -> (RawKeyPathComponent, Any.Type?) {
    let header = pop(RawKeyPathComponent.Header.self)
    // Track if this is the last component of the reference prefix.
    if header.endOfReferencePrefix {
      _sanityCheck(self.hasReferencePrefix,
                   "beginMutation marker in non-reference-writable key path?")
      self.hasReferencePrefix = false
    }
    
    let body: UnsafeRawBufferPointer
    let size = header.bodySize
    if size != 0 {
      body = popRaw(size)
    } else {
      body = UnsafeRawBufferPointer(start: nil, count: 0)
    }
    let component = RawKeyPathComponent(header: header, body: body)
    
    // fetch type, which is in the buffer unless it's the final component
    let nextType: Any.Type?
    if data.count == 0 {
      nextType = nil
    } else {
      if MemoryLayout<Any.Type>.size == 8 {
        // Words in the key path buffer are 32-bit aligned
        nextType = unsafeBitCast(pop((Int32, Int32).self),
                                 to: Any.Type.self)
      } else if MemoryLayout<Any.Type>.size == 4 {
        nextType = pop(Any.Type.self)
      } else {
        _sanityCheckFailure("unexpected word size")
      }
    }
    return (component, nextType)
  }
  
  mutating func pop<T>(_ type: T.Type) -> T {
    _sanityCheck(_isPOD(T.self), "should be POD")
    let raw = popRaw(MemoryLayout<T>.size)
    let resultBuf = UnsafeMutablePointer<T>.allocate(capacity: 1)
    _memcpy(dest: resultBuf,
            src: UnsafeMutableRawPointer(mutating: raw.baseAddress.unsafelyUnwrapped),
            size: UInt(MemoryLayout<T>.size))
    let result = resultBuf.pointee
    resultBuf.deallocate(capacity: 1)
    return result
  }
  mutating func popRaw(_ size: Int) -> UnsafeRawBufferPointer {
    _sanityCheck(data.count >= size,
                 "not enough space for next component?")
    let result = UnsafeRawBufferPointer(start: data.baseAddress, count: size)
    data = UnsafeRawBufferPointer(
      start: data.baseAddress.unsafelyUnwrapped + size,
      count: data.count - size
    )
    return result
  }
}

// MARK: Library intrinsics for projecting key paths.

public // COMPILER_INTRINSIC
func _projectKeyPathReadOnly<Root, Value>(
  root: Root,
  keyPath: KeyPath<Root, Value>
) -> Value {
  return keyPath.projectReadOnly(from: root)
}

public // COMPILER_INTRINSIC
func _projectKeyPathWritable<Root, Value>(
  root: UnsafeMutablePointer<Root>,
  keyPath: WritableKeyPath<Root, Value>
) -> (UnsafeMutablePointer<Value>, Builtin.NativeObject) {
  return keyPath.projectMutableAddress(from: root)
}

public // COMPILER_INTRINSIC
func _projectKeyPathReferenceWritable<Root, Value>(
  root: Root,
  keyPath: ReferenceWritableKeyPath<Root, Value>
) -> (UnsafeMutablePointer<Value>, Builtin.NativeObject) {
  return keyPath.projectMutableAddress(from: root)
}

// MARK: Appending type system

// FIXME(ABI): The type relationships between KeyPath append operands are tricky
// and don't interact well with our overriding rules. Hack things by injecting
// a bunch of `appending` overloads as protocol extensions so they aren't
// constrained by being overrides, and so that we can use exact-type constraints
// on `Self` to prevent dynamically-typed methods from being inherited by
// statically-typed key paths.
public protocol _AppendKeyPath {}

extension _AppendKeyPath where Self == AnyKeyPath {
  public func appending(path: AnyKeyPath) -> AnyKeyPath? {
    return _tryToAppendKeyPaths(root: self, leaf: path)
  }
}

extension _AppendKeyPath /* where Self == PartialKeyPath<T> */ {
  public func appending<Root>(path: AnyKeyPath) -> PartialKeyPath<Root>?
  where Self == PartialKeyPath<Root> {
    return _tryToAppendKeyPaths(root: self, leaf: path)
  }
  
  public func appending<Root, AppendedRoot, AppendedValue>(
    path: KeyPath<AppendedRoot, AppendedValue>
  ) -> KeyPath<Root, AppendedValue>?
  where Self == PartialKeyPath<Root> {
    return _tryToAppendKeyPaths(root: self, leaf: path)
  }
  
  public func appending<Root, AppendedRoot, AppendedValue>(
    path: ReferenceWritableKeyPath<AppendedRoot, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>?
  where Self == PartialKeyPath<Root> {
    return _tryToAppendKeyPaths(root: self, leaf: path)
  }
}

extension _AppendKeyPath /* where Self == KeyPath<T,U> */ {
  public func appending<Root, Value, AppendedValue>(
    path: KeyPath<Value, AppendedValue>
  ) -> KeyPath<Root, AppendedValue>
  where Self: KeyPath<Root, Value> {
    return _appendingKeyPaths(root: self, leaf: path)
  }

  /* TODO
  public func appending<Root, Value, Leaf>(
    path: Leaf,
    // FIXME: Satisfy "Value generic param not used in signature" constraint
    _: Value.Type = Value.self
  ) -> PartialKeyPath<Root>?
  where Self: KeyPath<Root, Value>, Leaf == AnyKeyPath {
    return _tryToAppendKeyPaths(root: self, leaf: path)
  }
   */

  public func appending<Root, Value, AppendedValue>(
    path: ReferenceWritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>
  where Self == KeyPath<Root, Value> {
    return _appendingKeyPaths(root: self, leaf: path)
  }
}

extension _AppendKeyPath /* where Self == WritableKeyPath<T,U> */ {
  public func appending<Root, Value, AppendedValue>(
    path: WritableKeyPath<Value, AppendedValue>
  ) -> WritableKeyPath<Root, AppendedValue>
  where Self == WritableKeyPath<Root, Value> {
    return _appendingKeyPaths(root: self, leaf: path)
  }

  public func appending<Root, Value, AppendedValue>(
    path: ReferenceWritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>
  where Self == WritableKeyPath<Root, Value> {
    return _appendingKeyPaths(root: self, leaf: path)
  }
}

extension _AppendKeyPath /* where Self == ReferenceWritableKeyPath<T,U> */ {
  public func appending<Root, Value, AppendedValue>(
    path: WritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>
  where Self == ReferenceWritableKeyPath<Root, Value> {
    return _appendingKeyPaths(root: self, leaf: path)
  }
}

// internal-with-availability
public func _tryToAppendKeyPaths<Result: AnyKeyPath>(
  root: AnyKeyPath,
  leaf: AnyKeyPath
) -> Result? {
  let (rootRoot, rootValue) = type(of: root)._rootAndValueType
  let (leafRoot, leafValue) = type(of: leaf)._rootAndValueType
  
  if rootValue != leafRoot {
    return nil
  }
  
  func open<Root>(_: Root.Type) -> Result {
    func open2<Value>(_: Value.Type) -> Result {
      func open3<AppendedValue>(_: AppendedValue.Type) -> Result {
        let typedRoot = unsafeDowncast(root, to: KeyPath<Root, Value>.self)
        let typedLeaf = unsafeDowncast(leaf,
                                       to: KeyPath<Value, AppendedValue>.self)
        let result = _appendingKeyPaths(root: typedRoot, leaf: typedLeaf)
        return unsafeDowncast(result, to: Result.self)
      }
      return _openExistential(leafValue, do: open3)
    }
    return _openExistential(rootValue, do: open2)
  }
  return _openExistential(rootRoot, do: open)
}

// internal-with-availability
public func _appendingKeyPaths<
  Root, Value, AppendedValue,
  Result: KeyPath<Root, AppendedValue>
>(
  root: KeyPath<Root, Value>,
  leaf: KeyPath<Value, AppendedValue>
) -> Result {
  let resultTy = type(of: root).appendedType(with: type(of: leaf))
  return root.withBuffer {
    var rootBuffer = $0
    return leaf.withBuffer {
      var leafBuffer = $0
      // Reserve room for the appended KVC string, if both key paths are
      // KVC-compatible.
      let appendedKVCLength: Int, rootKVCLength: Int, leafKVCLength: Int

      if let rootPtr = root._kvcKeyPathStringPtr,
         let leafPtr = leaf._kvcKeyPathStringPtr {
        rootKVCLength = Int(_swift_stdlib_strlen(rootPtr))
        leafKVCLength = Int(_swift_stdlib_strlen(leafPtr))
        // root + "." + leaf
        appendedKVCLength = rootKVCLength + 1 + leafKVCLength
      } else {
        rootKVCLength = 0
        leafKVCLength = 0
        appendedKVCLength = 0
      }

      // Result buffer has room for both key paths' components, plus the
      // header, plus space for the middle type.
      let resultSize = rootBuffer.data.count + leafBuffer.data.count
        + MemoryLayout<KeyPathBuffer.Header>.size
        + MemoryLayout<Int>.size
      // Tail-allocate space for the KVC string.
      let totalResultSize = (resultSize + appendedKVCLength + 3) & ~3

      var kvcStringBuffer: UnsafeMutableRawPointer? = nil

      let result = resultTy._create(capacityInBytes: totalResultSize) {
        var destBuffer = $0

        // Remember where the tail-allocated KVC string buffer begins.
        if appendedKVCLength > 0 {
          kvcStringBuffer = destBuffer.baseAddress.unsafelyUnwrapped
            .advanced(by: resultSize)

          destBuffer = .init(start: destBuffer.baseAddress,
                             count: resultSize)
        }
        
        func pushRaw(_ count: Int) {
          _sanityCheck(destBuffer.count >= count)
          destBuffer = UnsafeMutableRawBufferPointer(
            start: destBuffer.baseAddress.unsafelyUnwrapped + count,
            count: destBuffer.count - count
          )
        }
        func pushType(_ type: Any.Type) {
          let intSize = MemoryLayout<Int>.size
          _sanityCheck(destBuffer.count >= intSize)
          if intSize == 8 {
            let words = unsafeBitCast(type, to: (UInt32, UInt32).self)
            destBuffer.storeBytes(of: words.0,
                                  as: UInt32.self)
            destBuffer.storeBytes(of: words.1, toByteOffset: 4,
                                  as: UInt32.self)
          } else if intSize == 4 {
            destBuffer.storeBytes(of: type, as: Any.Type.self)
          } else {
            _sanityCheckFailure("unsupported architecture")
          }
          pushRaw(intSize)
        }
        
        // Save space for the header.
        let leafIsReferenceWritable = type(of: leaf).kind == .reference
        let header = KeyPathBuffer.Header(
          size: resultSize - MemoryLayout<KeyPathBuffer.Header>.size,
          trivial: rootBuffer.trivial && leafBuffer.trivial,
          hasReferencePrefix: rootBuffer.hasReferencePrefix
                              || leafIsReferenceWritable
        )
        destBuffer.storeBytes(of: header, as: KeyPathBuffer.Header.self)
        pushRaw(MemoryLayout<KeyPathBuffer.Header>.size)
        
        let leafHasReferencePrefix = leafBuffer.hasReferencePrefix
        
        // Clone the root components into the buffer.
        
        while true {
          let (component, type) = rootBuffer.next()
          let isLast = type == nil
          // If the leaf appended path has a reference prefix, then the
          // entire root is part of the reference prefix.
          let endOfReferencePrefix: Bool
          if leafHasReferencePrefix {
            endOfReferencePrefix = false
          } else if isLast && leafIsReferenceWritable {
            endOfReferencePrefix = true
          } else {
            endOfReferencePrefix = component.header.endOfReferencePrefix
          }
          
          component.clone(
            into: &destBuffer,
            endOfReferencePrefix: endOfReferencePrefix
          )
          if let type = type {
            pushType(type)
          } else {
            // Insert our endpoint type between the root and leaf components.
            pushType(Value.self)
            break
          }
        }
        
        // Clone the leaf components into the buffer.
        while true {
          let (component, type) = leafBuffer.next()

          component.clone(
            into: &destBuffer,
            endOfReferencePrefix: component.header.endOfReferencePrefix
          )

          if let type = type {
            pushType(type)
          } else {
            break
          }
        }
        
        _sanityCheck(destBuffer.count == 0,
                     "did not fill entire result buffer")
      }

      // Build the KVC string if there is one.
      if let kvcStringBuffer = kvcStringBuffer {
        let rootPtr = root._kvcKeyPathStringPtr.unsafelyUnwrapped
        let leafPtr = leaf._kvcKeyPathStringPtr.unsafelyUnwrapped
        _memcpy(dest: kvcStringBuffer,
                src: UnsafeMutableRawPointer(mutating: rootPtr),
                size: UInt(rootKVCLength))
        kvcStringBuffer.advanced(by: rootKVCLength)
          .storeBytes(of: 0x2E /* '.' */, as: CChar.self)
        _memcpy(dest: kvcStringBuffer.advanced(by: rootKVCLength + 1),
                src: UnsafeMutableRawPointer(mutating: leafPtr),
                size: UInt(leafKVCLength))
        result._kvcKeyPathStringPtr =
          UnsafePointer(kvcStringBuffer.assumingMemoryBound(to: CChar.self))
        kvcStringBuffer.advanced(by: rootKVCLength + leafKVCLength + 1)
          .storeBytes(of: 0 /* '\0' */, as: CChar.self)
      }
      return unsafeDowncast(result, to: Result.self)
    }
  }
}

// The distance in bytes from the address point of a KeyPath object to its
// buffer header. Includes the size of the Swift heap object header and the
// pointer to the KVC string.

internal var keyPathObjectHeaderSize: Int {
  return MemoryLayout<HeapObject>.size + MemoryLayout<Int>.size
}

// Runtime entry point to instantiate a key path object.
@_cdecl("swift_getKeyPath")
public func _swift_getKeyPath(pattern: UnsafeMutableRawPointer,
                              arguments: UnsafeRawPointer)
    -> UnsafeRawPointer {
  // The key path pattern is laid out like a key path object, with a few
  // modifications:
  // - Instead of the two-word object header with isa and refcount, two
  //   pointers to metadata accessors are provided for the root and leaf
  //   value types of the key path.
  // - The header reuses the "trivial" bit to mean "instantiable in-line",
  //   meaning that the key path described by this pattern has no contextually
  //   dependent parts (no dependence on generic parameters, subscript indexes,
  //   etc.), so it can be set up as a global object once. (The resulting
  //   global object will itself always have the "trivial" bit set, since it
  //   never needs to be destroyed.)
  // - Components may have unresolved forms that require instantiation.
  // - Type metadata pointers are unresolved, and instead
  //   point to accessor functions that instantiate the metadata.
  //
  // The pattern never precomputes the capabilities of the key path (readonly/
  // writable/reference-writable), nor does it encode the reference prefix.
  // These are resolved dynamically, so that they always reflect the dynamic
  // capability of the properties involved.
  let oncePtr = pattern
  let patternPtr = pattern.advanced(by: MemoryLayout<Int>.size)
  let bufferPtr = patternPtr.advanced(by: keyPathObjectHeaderSize)

  // If the pattern is instantiable in-line, do a dispatch_once to
  // initialize it. (The resulting object will still have the collocated
  // "trivial" bit set, since a global object never needs destruction.)
  let bufferHeader = bufferPtr.load(as: KeyPathBuffer.Header.self)
  bufferHeader.validateReservedBits()

  if bufferHeader.instantiableInLine {
    Builtin.onceWithContext(oncePtr._rawValue, _getKeyPath_instantiateInline,
                            patternPtr._rawValue)
    // Return the instantiated object at +1.
    // TODO: This will be unnecessary once we support global objects with inert
    // refcounting.
    let object = Unmanaged<AnyKeyPath>.fromOpaque(patternPtr)
    _ = object.retain()
    return UnsafeRawPointer(patternPtr)
  }

  // Otherwise, instantiate a new key path object modeled on the pattern.
  return _getKeyPath_instantiatedOutOfLine(patternPtr, arguments)
}

internal func _getKeyPath_instantiatedOutOfLine(
  _ pattern: UnsafeRawPointer,
  _ arguments: UnsafeRawPointer)
    -> UnsafeRawPointer {
  // Do a pass to determine the class of the key path we'll be instantiating
  // and how much space we'll need for it.
  let (keyPathClass, rootType, size)
    = _getKeyPathClassAndInstanceSize(pattern, arguments)

  // Allocate the instance.
  let instance = keyPathClass._create(capacityInBytes: size) { instanceData in
    // Instantiate the pattern into the instance.
    let patternBufferPtr = pattern.advanced(by: keyPathObjectHeaderSize)
    let patternBuffer = KeyPathBuffer(base: patternBufferPtr)

    _instantiateKeyPathBuffer(patternBuffer, instanceData, rootType, arguments)
  }
  // Take the KVC string from the pattern.
  let kvcStringPtr = pattern.advanced(by: MemoryLayout<HeapObject>.size)
  instance._kvcKeyPathStringPtr = kvcStringPtr
    .load(as: Optional<UnsafePointer<CChar>>.self)

  // Hand it off at +1.
  return UnsafeRawPointer(Unmanaged.passRetained(instance).toOpaque())
}

internal func _getKeyPath_instantiateInline(
  _ objectRawPtr: Builtin.RawPointer
) {
  let objectPtr = UnsafeMutableRawPointer(objectRawPtr)

  // Do a pass to determine the class of the key path we'll be instantiating
  // and how much space we'll need for it.
  // The pattern argument doesn't matter since an in-place pattern should never
  // have arguments.
  let (keyPathClass, rootType, instantiatedSize)
    = _getKeyPathClassAndInstanceSize(objectPtr, objectPtr)

  // TODO: Eventually, we'll need to handle cases where the instantiated
  // key path has a different size from the pattern (because it involves
  // resilient types, for example). For now, require that the size match the
  // buffer.

  let bufferPtr = objectPtr.advanced(by: keyPathObjectHeaderSize)
  let buffer = KeyPathBuffer(base: bufferPtr)
  let totalSize = buffer.data.count + MemoryLayout<KeyPathBuffer.Header>.size
  let bufferData = UnsafeMutableRawBufferPointer(
    start: bufferPtr,
    count: totalSize)

  _sanityCheck(instantiatedSize == totalSize,
               "size-changing in-place instantiation not implemented")

  // Instantiate the pattern in place.
  _instantiateKeyPathBuffer(buffer, bufferData, rootType, bufferPtr)

  _swift_instantiateInertHeapObject(objectPtr,
    unsafeBitCast(keyPathClass, to: OpaquePointer.self))
}

internal typealias MetadataAccessor =
  @convention(c) (UnsafeRawPointer) -> UnsafeRawPointer

internal func _getKeyPathClassAndInstanceSize(
  _ pattern: UnsafeRawPointer,
  _ arguments: UnsafeRawPointer
) -> (
  keyPathClass: AnyKeyPath.Type,
  rootType: Any.Type,
  size: Int
) {
  // Resolve the root and leaf types.
  let rootAccessor = pattern.load(as: MetadataAccessor.self)
  let leafAccessor = pattern.load(fromByteOffset: MemoryLayout<Int>.size,
                                    as: MetadataAccessor.self)

  let root = unsafeBitCast(rootAccessor(arguments), to: Any.Type.self)
  let leaf = unsafeBitCast(leafAccessor(arguments), to: Any.Type.self)

  // Scan the pattern to figure out the dynamic capability of the key path.
  // Start off assuming the key path is writable.
  var capability: KeyPathKind = .value

  let bufferPtr = pattern.advanced(by: keyPathObjectHeaderSize)
  var buffer = KeyPathBuffer(base: bufferPtr)
  let size = buffer.data.count + MemoryLayout<KeyPathBuffer.Header>.size

  scanComponents: while true {
    let header = buffer.pop(RawKeyPathComponent.Header.self)

    func popOffset() {
      if header.payload == RawKeyPathComponent.Header.unresolvedOffsetPayload
        || header.payload == RawKeyPathComponent.Header.outOfLineOffsetPayload {
        _ = buffer.pop(UInt32.self)
      }
    }

    switch header.kind {
    case .struct:
      // No effect on the capability.
      // TODO: we should dynamically prevent "let" properties from being
      // reassigned.
      popOffset()
    case .class:
      // The rest of the key path could be reference-writable.
      // TODO: we should dynamically prevent "let" properties from being
      // reassigned.
      capability = .reference
      popOffset()
    case .computed:
      let settable =
        header.payload & RawKeyPathComponent.Header.computedSettableFlag != 0
      let mutating =
        header.payload & RawKeyPathComponent.Header.computedMutatingFlag != 0

      switch (settable, mutating) {
      case (false, false):
        // If the property is get-only, the capability becomes read-only, unless
        // we get another reference-writable component.
        capability = .readOnly
      case (true, false):
        capability = .reference
      case (true, true):
        // Writable if the base is. No effect.
        break
      case (false, true):
        _sanityCheckFailure("unpossible")
      }

      _sanityCheck(
        header.payload & RawKeyPathComponent.Header.computedHasArgumentsFlag == 0,
        "arguments not implemented yet")

      _ = buffer.popRaw(MemoryLayout<Int>.size * (settable ? 3 : 2))

    case .optionalChain,
         .optionalWrap:
      // Chaining always renders the whole key path read-only.
      capability = .readOnly
      break scanComponents

    case .optionalForce:
      // No effect.
      break
    }

    // Break if this is the last component.
    if buffer.data.count == 0 { break }

    // Pop the type accessor reference.
    _ = buffer.popRaw(MemoryLayout<Int>.size)
  }

  // Grab the class object for the key path type we'll end up with.
  func openRoot<Root>(_: Root.Type) -> AnyKeyPath.Type {
    func openLeaf<Leaf>(_: Leaf.Type) -> AnyKeyPath.Type {
      switch capability {
      case .readOnly:
        return KeyPath<Root, Leaf>.self
      case .value:
        return WritableKeyPath<Root, Leaf>.self
      case .reference:
        return ReferenceWritableKeyPath<Root, Leaf>.self
      }
    }
    return _openExistential(leaf, do: openLeaf)
  }
  let classTy = _openExistential(root, do: openRoot)

  return (keyPathClass: classTy, rootType: root, size: size)
}

internal func _instantiateKeyPathBuffer(
  _ origPatternBuffer: KeyPathBuffer,
  _ origDestData: UnsafeMutableRawBufferPointer,
  _ rootType: Any.Type,
  _ arguments: UnsafeRawPointer
) {
  // NB: patternBuffer and destData alias when the pattern is instantiable
  // in-line. Therefore, do not read from patternBuffer after the same position
  // in destData has been written to.

  var patternBuffer = origPatternBuffer
  let destHeaderPtr = origDestData.baseAddress.unsafelyUnwrapped
  _sanityCheck(origDestData.count >= MemoryLayout<KeyPathBuffer.Header>.size)
  var destData = UnsafeMutableRawBufferPointer(
    start: destHeaderPtr.advanced(by: MemoryLayout<KeyPathBuffer.Header>.size),
    count: origDestData.count - MemoryLayout<KeyPathBuffer.Header>.size)

  func pushDest<T>(_ value: T) {
    _sanityCheck(_isPOD(T.self))
    var value2 = value
    let size = MemoryLayout<T>.size
    _sanityCheck(destData.count >= size)
    _memcpy(dest: destData.baseAddress.unsafelyUnwrapped, src: &value2,
            size: UInt(size))
    destData = UnsafeMutableRawBufferPointer(
      start: destData.baseAddress.unsafelyUnwrapped.advanced(by: size),
      count: destData.count - size)
  }

  // Track where the reference prefix begins.
  var endOfReferencePrefixComponent: UnsafeMutableRawPointer? = nil
  var previousComponentAddr: UnsafeMutableRawPointer? = nil

  // Instantiate components that need it.
  var base: Any.Type = rootType
  while true {
    let componentAddr = destData.baseAddress.unsafelyUnwrapped
    let header = patternBuffer.pop(RawKeyPathComponent.Header.self)

    func tryToResolveOffset() {
      if header.payload == RawKeyPathComponent.Header.unresolvedOffsetPayload {
        // Look up offset in type metadata. The value in the pattern is the
        // offset within the metadata object.
        let metadataPtr = unsafeBitCast(base, to: UnsafeRawPointer.self)
        let offsetOfOffset = patternBuffer.pop(UInt32.self)
        let offset = metadataPtr.load(fromByteOffset: Int(offsetOfOffset),
                                      as: UInt32.self)
        // Rewrite the header for a resolved offset.
        var newHeader = header
        newHeader.payload = RawKeyPathComponent.Header.outOfLineOffsetPayload
        pushDest(newHeader)
        pushDest(offset)
        return
      }

      // Otherwise, just transfer the pre-resolved component.
      pushDest(header)
      if header.payload == RawKeyPathComponent.Header.outOfLineOffsetPayload {
        let offset = patternBuffer.pop(UInt32.self)
        pushDest(offset)
      }
    }

    switch header.kind {
    case .struct:
      // The offset may need to be resolved dynamically.
      tryToResolveOffset()
    case .class:
      // Crossing a class can end the reference prefix, and makes the following
      // key path potentially reference-writable.
      endOfReferencePrefixComponent = previousComponentAddr
      // The offset may need to be resolved dynamically.
      tryToResolveOffset()
    case .optionalChain,
         .optionalWrap,
         .optionalForce:
      // No instantiation necessary.
      pushDest(header)
      break
    case .computed:
      // A nonmutating settable property can end the reference prefix and
      // makes the following key path potentially reference-writable.
      if header.payload & RawKeyPathComponent.Header.computedSettableFlag != 0
         && header.payload & RawKeyPathComponent.Header.computedMutatingFlag == 0 {
        endOfReferencePrefixComponent = previousComponentAddr
      }

      // The offset may need resolution if the property is keyed by a stored
      // property.
      var newHeader = header
      var id = patternBuffer.pop(Int.self)
      switch header.payload
                         & RawKeyPathComponent.Header.computedIDResolutionMask {
      case RawKeyPathComponent.Header.computedIDResolved:
        // Nothing to do.
        break
      case RawKeyPathComponent.Header.computedIDUnresolvedFieldOffset:
        // The value in the pattern is an offset into the type metadata that
        // points to the field offset for the stored property identifying the
        // component.
        _sanityCheck(header.payload
            & RawKeyPathComponent.Header.computedIDByStoredPropertyFlag != 0,
          "only stored property IDs should need offset resolution")
        let metadataPtr = unsafeBitCast(base, to: UnsafeRawPointer.self)
        id = metadataPtr.load(fromByteOffset: id, as: Int.self)
      case RawKeyPathComponent.Header.computedIDUnresolvedIndirectPointer:
        // The value in the pattern is a pointer to the actual unique word-sized
        // value in memory.
        let idPtr = UnsafeRawPointer(bitPattern: id).unsafelyUnwrapped
        id = idPtr.load(as: Int.self)
      default:
        _sanityCheckFailure("unpossible")
      }
      newHeader.payload &= ~RawKeyPathComponent.Header.computedIDResolutionMask
      pushDest(newHeader)
      pushDest(id)
      // Carry over the accessors.
      let getter = patternBuffer.pop(UnsafeRawPointer.self)
      pushDest(getter)
      if header.payload & RawKeyPathComponent.Header.computedSettableFlag != 0{
        let setter = patternBuffer.pop(UnsafeRawPointer.self)
        pushDest(setter)
      }
      // Carry over the arguments.
      _sanityCheck(header.payload
          & RawKeyPathComponent.Header.computedHasArgumentsFlag == 0,
        "arguments not implemented")
    }

    // Break if this is the last component.
    if patternBuffer.data.count == 0 { break }

    // Resolve the component type.
    if MemoryLayout<Int>.size == 4 {
      let componentTyAccessor = patternBuffer.pop(MetadataAccessor.self)
      base = unsafeBitCast(componentTyAccessor(arguments), to: Any.Type.self)
      pushDest(base)
    } else if MemoryLayout<Int>.size == 8 {
      let componentTyAccessorWords = patternBuffer.pop((UInt32,UInt32).self)
      let componentTyAccessor = unsafeBitCast(componentTyAccessorWords,
                                              to: MetadataAccessor.self)
      base = unsafeBitCast(componentTyAccessor(arguments), to: Any.Type.self)
      let componentTyWords = unsafeBitCast(base,
                                           to: (UInt32, UInt32).self)
      pushDest(componentTyWords)
    } else {
      fatalError("unsupported architecture")
    }
    previousComponentAddr = componentAddr
  }

  // We should have traversed both buffers.
  _sanityCheck(patternBuffer.data.isEmpty && destData.isEmpty)

  // Write out the header.
  let destHeader = KeyPathBuffer.Header(size: origPatternBuffer.data.count,
    trivial: true, // TODO: nontrivial indexes
    hasReferencePrefix: endOfReferencePrefixComponent != nil)

  destHeaderPtr.storeBytes(of: destHeader, as: KeyPathBuffer.Header.self)

  // Mark the reference prefix if there is one.
  if let endOfReferencePrefixComponent = endOfReferencePrefixComponent {
    var componentHeader = endOfReferencePrefixComponent
      .load(as: RawKeyPathComponent.Header.self)
    componentHeader.endOfReferencePrefix = true
    endOfReferencePrefixComponent.storeBytes(of: componentHeader,
      as: RawKeyPathComponent.Header.self)
  }
}
