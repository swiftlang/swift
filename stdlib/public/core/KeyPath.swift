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

#if BUILDING_OUTSIDE_STDLIB
import Swift

internal func _conditionallyUnreachable() -> Never {
  _conditionallyUnreachable()
}

extension Array {
  func _getOwner_native() -> Builtin.NativeObject { fatalError() }
}
#endif

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

public class AnyKeyPath: Hashable {
  public func appending<Value, AppendedValue>(
    path: KeyPath<Value, AppendedValue>
  ) -> AnyKeyPath? {
    _abstract()
  }
  public class var rootType: Any.Type {
    _abstract()
  }
  public class var valueType: Any.Type {
    _abstract()
  }
  
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
  
  // MARK: Implementation details
  
  // Prevent normal initialization. We use tail allocation via
  // allocWithTailElems().
  internal init() {
    _sanityCheckFailure("use _create(...)")
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

public class PartialKeyPath<Root>: AnyKeyPath {
  public override func appending<Value, AppendedValue>(
    path: KeyPath<Value, AppendedValue>
  ) -> KeyPath<Root, AppendedValue>? {
    _abstract()
  }
  public func appending<Value, AppendedValue>(
    path: ReferenceWritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>? {
    _abstract()
  }

  // MARK: Override abstract interfaces
  @_inlineable
  public final override class var rootType: Any.Type {
    return Root.self
  }
}

// MARK: Concrete implementations

public class KeyPath<Root, Value>: PartialKeyPath<Root> {
  public typealias _Root = Root
  public typealias _Value = Value

  public func appending<AppendedValue>(
    path: KeyPath<Value, AppendedValue>
  ) -> KeyPath<Root, AppendedValue> {
    let resultTy = type(of: self).appendedType(with: type(of: path))
    return withBuffer {
      var rootBuffer = $0
      return path.withBuffer {
        var leafBuffer = $0
        // Result buffer has room for both key paths' components, plus the
        // header, plus space for the middle type.
        let resultSize = rootBuffer.data.count + leafBuffer.data.count
          + MemoryLayout<KeyPathBuffer.Header>.size
          + MemoryLayout<Int>.size
        return resultTy._create(capacityInBytes: resultSize) {
          var destBuffer = $0
          
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
          let leafIsReferenceWritable = type(of: path).kind == .reference
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
      }
    }
  }

  @_inlineable
  public final func appending<AppendedValue>(
    path: ReferenceWritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue> {
    return unsafeDowncast(
      appending(path: path as KeyPath<Value, AppendedValue>),
      to: ReferenceWritableKeyPath<Root, AppendedValue>.self
    )
  }

  // MARK: Override optional-returning abstract interfaces
  @_inlineable
  public final override func appending<Value2, AppendedValue>(
    path: KeyPath<Value2, AppendedValue>
  ) -> KeyPath<Root, AppendedValue>? {
    if Value2.self == Value.self {
      return .some(appending(
        path: unsafeDowncast(path, to: KeyPath<Value, AppendedValue>.self)))
    }
    return nil
  }
  
  @_inlineable
  public final override func appending<Value2, AppendedValue>(
    path: ReferenceWritableKeyPath<Value2, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>? {
    if Value2.self == Value.self {
      return .some(appending(
        path: unsafeDowncast(path,
                      to: ReferenceWritableKeyPath<Value, AppendedValue>.self)))
    }
    return nil
  }

  @_inlineable
  public final override class var valueType: Any.Type {
    return Value.self
  }
  
  // MARK: Implementation
  
  enum Kind { case readOnly, value, reference }
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

public class WritableKeyPath<Root, Value>: KeyPath<Root, Value> {
  public final func appending<AppendedValue>(
    path: WritableKeyPath<Value, AppendedValue>
  ) -> WritableKeyPath<Root, AppendedValue> {
    return unsafeDowncast(
      appending(path: path as KeyPath<Value, AppendedValue>),
      to: WritableKeyPath<Root, AppendedValue>.self
    )
  }

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

public class ReferenceWritableKeyPath<Root, Value>: WritableKeyPath<Root, Value> {
  /* TODO: need a "new" attribute */
  @_inlineable
  public final func appendingR<AppendedValue>(
    path: WritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue> {
    return unsafeDowncast(
      appending(path: path as KeyPath<Value, AppendedValue>),
      to: ReferenceWritableKeyPath<Root, AppendedValue>.self
    )
  }

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

enum KeyPathComponentKind {
  /// The keypath projects within the storage of the outer value, like a
  /// stored property in a struct.
  case `struct`
  /// The keypath projects from the referenced pointer, like a
  /// stored property in a class.
  case `class`
  /// The keypath optional-chains, returning nil immediately if the input is
  /// nil, or else proceeding by projecting the value inside.
  case optionalChain
  /// The keypath optional-forces, trapping if the input is
  /// nil, or else proceeding by projecting the value inside.
  case optionalForce
  /// The keypath wraps a value in an optional.
  case optionalWrap
}

enum KeyPathComponent: Hashable {
  struct RawAccessor {
    var rawCode: Builtin.RawPointer
    var rawContext: Builtin.NativeObject?
  }
  /// The keypath projects within the storage of the outer value, like a
  /// stored property in a struct.
  case `struct`(offset: Int)
  /// The keypath projects from the referenced pointer, like a
  /// stored property in a class.
  case `class`(offset: Int)
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
    case (.struct, _),
         (.class,  _),
         (.optionalChain, _),
         (.optionalForce, _),
         (.optionalWrap, _):
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
    }
    return hash
  }
}

struct RawKeyPathComponent {
  var header: Header
  var body: UnsafeRawBufferPointer
  
  struct Header {
    static var payloadBits: Int { return 29 }
    static var payloadMask: Int { return 0xFFFF_FFFF >> (32 - payloadBits) }
    
    var _value: UInt32
    
    var discriminator: Int { return Int(_value) >> Header.payloadBits & 0x3 }
    var payload: Int {
      get { return Int(_value) & Header.payloadMask }
      set {
        _sanityCheck(newValue & Header.payloadMask == newValue,
                     "payload too big")
        let shortMask = UInt32(Header.payloadMask)
        _value = _value & ~shortMask | UInt32(newValue)
      }
    }
    var endOfReferencePrefix: Bool {
      get {
        return Int(_value) >> Header.payloadBits & 0x4 != 0
      }
      set {
        let bit = 0x4 << UInt32(Header.payloadBits)
        if newValue {
          _value = _value | bit
        } else {
          _value = _value & ~bit
        }
      }
    }

    var kind: KeyPathComponentKind {
      switch (discriminator, payload) {
      case (0, _):
        return .struct
      case (2, _):
        return .class
      case (3, 0):
        return .optionalChain
      case (3, 1):
        return .optionalWrap
      case (3, 2):
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
      }
    }
    
    var isTrivial: Bool {
      switch kind {
      case .struct, .class, .optionalChain, .optionalForce, .optionalWrap:
        return true
      }
    }
  }

  var _structOrClassOffset: Int {
    _sanityCheck(header.kind == .struct || header.kind == .class,
                 "no offset for this kind")
    // An offset too large to fit inline is represented by a signal and stored
    // in the body.
    if header.payload == Header.payloadMask {
      // Offset overflowed into body
      _sanityCheck(body.count >= MemoryLayout<UInt32>.size,
                   "component not big enough")
      return Int(body.load(as: UInt32.self))
    }
    return header.payload
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
      if header.payload == Header.payloadMask {
        let overflowOffset = body.load(as: UInt32.self)
        buffer.storeBytes(of: overflowOffset, toByteOffset: 4,
                          as: UInt32.self)
        componentSize += 4
      }
    case .optionalChain,
         .optionalForce,
         .optionalWrap:
      break
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
    
    case .optionalForce:
      fatalError("TODO")
    
    case .optionalChain, .optionalWrap:
      _sanityCheckFailure("not a mutable key path component")
    }
  }
}

internal struct KeyPathBuffer {
  var data: UnsafeRawBufferPointer
  var trivial: Bool
  var hasReferencePrefix: Bool
  
  struct Header {
    var _value: UInt32
    
    init(size: Int, trivial: Bool, hasReferencePrefix: Bool) {
      _sanityCheck(size <= 0x3FFF_FFFF, "key path too big")
      _value = UInt32(size)
        | (trivial ? 0x8000_0000 : 0)
        | (hasReferencePrefix ? 0x4000_0000 : 0)
    }
    
    var size: Int { return Int(_value & 0x3FFF_FFFF) }
    var trivial: Bool { return _value & 0x8000_0000 != 0 }
    var hasReferencePrefix: Bool { return _value & 0x4000_0000 != 0 }
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
    let raw = popRaw(MemoryLayout<T>.size)
    return raw.load(as: type)
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

public struct _KeyPathBase<T> {
  public var base: T
  public init(base: T) { self.base = base }
  
  // TODO: These subscripts ought to sit on `Any`
  public subscript<U>(keyPath: KeyPath<T, U>) -> U {
    return keyPath.projectReadOnly(from: base)
  }
  
  public subscript<U>(keyPath: WritableKeyPath<T, U>) -> U {
    get {
      return keyPath.projectReadOnly(from: base)
    }
    mutableAddressWithNativeOwner {
      // The soundness of this `addressof` operation relies on the returned
      // address from an address only being used during a single formal access
      // of `self` (IOW, there's no end of the formal access between
      // `materializeForSet` and its continuation).
      let basePtr = UnsafeMutablePointer<T>(Builtin.addressof(&base))
      return keyPath.projectMutableAddress(from: basePtr)
    }
  }

  public subscript<U>(keyPath: ReferenceWritableKeyPath<T, U>) -> U {
    get {
      return keyPath.projectReadOnly(from: base)
    }
    nonmutating mutableAddressWithNativeOwner {
      return keyPath.projectMutableAddress(from: base)
    }
  }
}
