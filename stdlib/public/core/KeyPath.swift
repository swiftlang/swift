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
    fatalError("TODO")
  }
  public static func ==(a: AnyKeyPath, b: AnyKeyPath) -> Bool {
    fatalError("TODO")
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
  public func appending<AppendedValue>(
    path: KeyPath<Value, AppendedValue>
  ) -> KeyPath<Root, AppendedValue> {
    let resultTy = KeyPath.appendedType(with: type(of: path))
    return withBuffer { selfBuffer in
      return path.withBuffer { pathBuffer in
        // TODO: return resultTy.create....
        _ = resultTy
        fatalError()
      }
    }
  }

  public final func appending<AppendedValue>(
    path: WritableKeyPath<Value, AppendedValue>
  ) -> Self {
    return unsafeDowncast(appending(path: path as KeyPath<Value, AppendedValue>),
                          to: type(of: self))
  }
  
  public final func appending<AppendedValue>(
    path: ReferenceWritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue> {
    return unsafeDowncast(appending(path: path as KeyPath<Value, AppendedValue>),
                          to: ReferenceWritableKeyPath<Root, AppendedValue>.self)
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
  /// The keypath has a getter.
  case get
  /// The keypath has a getter and setter.
  case getSet
}

enum KeyPathComponent {
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
  /// The keypath is read-only using a getter function.
  case get(RawAccessor)
  /// The keypath is read-only using a getter/setter pair.
  case getSet(getter: RawAccessor, setter: RawAccessor, isMutating: Bool)
}

struct RawKeyPathComponent {
  var header: Header
  var body: UnsafeRawBufferPointer
  
  struct Header {
    static var payloadBits: Int { return 29 }
    static var payloadMask: Int { return 0xFFFF_FFFF >> (32 - payloadBits) }
    
    var _value: UInt32
    
    var discriminator: Int { return Int(_value) >> Header.payloadBits & 0x3 }
    var payload: Int { return Int(_value) & Header.payloadMask }
    var lastComponentOfReferencePrefix: Bool {
      return Int(_value) >> Header.payloadBits & 0x4 != 0
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
        return .optionalForce
      case (3, 2):
        return .get
      case (3, 3), // nonmutating
           (3, 4): // mutating
        return .getSet
      default:
        _sanityCheckFailure("invalid header")
      }
    }
    
    var bodySize: Int {
      switch kind {
      case .struct, .class:
        if payload == Header.payloadMask { return 4 } // overflowed
        return 0
      case .optionalChain, .optionalForce:
        return 0
      case .get:
        return MemoryLayout<Int>.size * 2
      case .getSet:
        // 32 bits of context are packed in the header payload
        return MemoryLayout<Int>.size * 4
      }
    }
    
    var isTrivial: Bool {
      switch kind {
      case .struct, .class, .optionalChain, .optionalForce:
        return true
      case .get, .getSet:
        // Need to release context pointers for the getter and setter
        return false
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
  
  func _getSetWord(at i: Int) -> Builtin.RawPointer? {
    let result: Builtin.RawPointer?
    if MemoryLayout<UInt>.size == 8 {
      // Components are only 4-byte-aligned so we need to load two words
      // separately.
      let firstWordOffset = MemoryLayout<UInt32>.size * 2*i
      _sanityCheck(body.count >= firstWordOffset + MemoryLayout<UInt32>.size*2,
                   "component not big enough")
      let lowWord = body.load(fromByteOffset: firstWordOffset,
                              as: UInt32.self)
      let highWord = body.load(fromByteOffset: firstWordOffset+4,
                               as: UInt32.self)
      let rawAddr = UInt(lowWord) | UInt(highWord) << 32
      result = rawAddr == 0
        ? nil : Builtin.inttoptr_Word(rawAddr._builtinWordValue)
    } else if MemoryLayout<UInt>.size == 4 {
      // Context fits in header; code pointers use the entire body.
      let firstWordOffset = MemoryLayout<UInt32>.size * i
      _sanityCheck(body.count >= firstWordOffset + MemoryLayout<UInt32>.size,
                   "component not big enough")
      result = body.load(fromByteOffset: firstWordOffset,
                         as: Optional<Builtin.RawPointer>.self)
    } else {
      _sanityCheckFailure("unimplemented architecture")
    }
    return result
  }
  
  var _getterContext: Builtin.NativeObject? {
    _sanityCheck(header.kind == .getSet,
                 "no setter context for this kind")
    return _getSetWord(at: 0).map { Builtin.bridgeFromRawPointer($0) }
  }
  var _getterRawCode: Builtin.RawPointer {
    _sanityCheck(header.kind == .get || header.kind == .getSet,
                 "no getter code for this kind")
    return _getSetWord(at: 1).unsafelyUnwrapped
  }
  var _setterContext: Builtin.NativeObject? {
    _sanityCheck(header.kind == .getSet,
                 "no setter context for this kind")
    return _getSetWord(at: 2).map { Builtin.bridgeFromRawPointer($0) }
  }
  var _setterRawCode: Builtin.RawPointer {
    _sanityCheck(header.kind == .getSet,
                 "no setter code for this kind")
    return _getSetWord(at: 3).unsafelyUnwrapped
  }

  var value: KeyPathComponent {
    return dump({ switch header.kind {
    case .struct:
      return .struct(offset: _structOrClassOffset)
    case .class:
      return .class(offset: _structOrClassOffset)
    case .optionalChain:
      return .optionalChain
    case .get:
      return .get(.init(rawCode: _getterRawCode, rawContext: _getterContext))
    case .getSet:
      _sanityCheck(header.payload == 3 || header.payload == 4,
                   "invalid header for getSet component")
      return .getSet(
        getter: .init(rawCode: _getterRawCode, rawContext: _getterContext),
        setter: .init(rawCode: _setterRawCode, rawContext: _setterContext),
        isMutating: header.payload == 4
      )
    case .optionalForce:
      return .optionalForce
    } }())
  }

  func destroy() {
    switch header.kind {
    case .struct,
         .class,
         .optionalChain,
         .optionalForce:
      // trivial
      return
    case .getSet:
      // need to release contexts
      if let context = _setterContext {
        Builtin.release(context)
      }
      fallthrough
    case .get:
      if let context = _getterContext {
        Builtin.release(context)
      }
    }
  }
  
  func clone(into buffer: UnsafeMutableRawPointer) {
    fatalError("todo")
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
    case .get(let getter),
         .getSet(let getter, setter: _, isMutating: _):
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
      
    case .getSet:
      fatalError("TODO")
    case .optionalForce:
      fatalError("TODO")

    case .optionalChain, .get:
      _sanityCheckFailure("not a mutable key path component")
    }
  }
}

internal struct KeyPathBuffer {
  var data: UnsafeRawBufferPointer
  var trivial: Bool
  var hasReferencePrefix: Bool
  
  init(base: UnsafeRawPointer) {
    struct Header {
      var _value: UInt32
      
      var size: Int { return Int(_value & 0x3FFF_FFFF) }
      var trivial: Bool { return _value & 0x8000_0000 != 0 }
      var hasReferencePrefix: Bool { return _value & 0x4000_0000 != 0 }
    }

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
    if header.lastComponentOfReferencePrefix {
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
