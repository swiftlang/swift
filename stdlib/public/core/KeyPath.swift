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
  @inlinable
  public static var rootType: Any.Type {
    return _rootAndValueType.root
  }

  /// The value type for this key path.
  @inlinable
  public static var valueType: Any.Type {
    return _rootAndValueType.value
  }

  internal final var _kvcKeyPathStringPtr: UnsafePointer<CChar>?
  
  /// The hash value.
  final public var hashValue: Int {
    return _hashValue(for: self)
  }

  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @_effects(releasenone)
  final public func hash(into hasher: inout Hasher) {
    ObjectIdentifier(type(of: self)).hash(into: &hasher)
    return withBuffer {
      var buffer = $0
      while true {
        let (component, type) = buffer.next()
        hasher.combine(component.value)
        if let type = type {
          hasher.combine(unsafeBitCast(type, to: Int.self))
        } else {
          break
        }
      }
    }
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

  @usableFromInline
  internal class var _rootAndValueType: (root: Any.Type, value: Any.Type) {
    _abstract()
  }
  
  internal static func _create(
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
  
  internal func withBuffer<T>(_ f: (KeyPathBuffer) throws -> T) rethrows -> T {
    defer { _fixLifetime(self) }
    
    let base = UnsafeRawPointer(Builtin.projectTailElems(self, Int32.self))
    return try f(KeyPathBuffer(base: base))
  }

  @usableFromInline // Exposed as public API by MemoryLayout<Root>.offset(of:)
  internal var _storedInlineOffset: Int? {
    return withBuffer {
      var buffer = $0
      var offset = 0
      while true {
        let (rawComponent, optNextType) = buffer.next()
        switch rawComponent.header.kind {
        case .struct:
          offset += rawComponent._structOrClassOffset

        case .class, .computed, .optionalChain, .optionalForce, .optionalWrap, .external:
          return .none
        }

        if optNextType == nil { return .some(offset) }
      }
    }
  }
}

/// A partially type-erased key path, from a concrete root type to any
/// resulting value type.
public class PartialKeyPath<Root>: AnyKeyPath { }

// MARK: Concrete implementations
internal enum KeyPathKind { case readOnly, value, reference }

/// A key path from a specific root type to a specific resulting value type.
public class KeyPath<Root, Value>: PartialKeyPath<Root> {
  @usableFromInline
  internal final override class var _rootAndValueType: (
    root: Any.Type,
    value: Any.Type
  ) {
    return (Root.self, Value.self)
  }
  
  // MARK: Implementation
  internal typealias Kind = KeyPathKind
  internal class var kind: Kind { return .readOnly }
  
  internal static func appendedType<AppendedValue>(
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
  
  @usableFromInline
  internal final func projectReadOnly(from root: Root) -> Value {
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
            switch rawComponent.projectReadOnly(base,
              to: NewValue.self, endingWith: Value.self) {
            case .continue(let newBase):
              if isLast {
                _sanityCheck(NewValue.self == Value.self,
                             "key path does not terminate in correct type")
                return unsafeBitCast(newBase, to: Value.self)
              } else {
                curBase = newBase
                return nil
              }
            case .break(let result):
              return result
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
  
  internal override class var kind: Kind { return .value }

  // `base` is assumed to be undergoing a formal access for the duration of the
  // call, so must not be mutated by an alias
  @usableFromInline
  internal func projectMutableAddress(from base: UnsafePointer<Root>)
      -> (pointer: UnsafeMutablePointer<Value>, owner: AnyObject?) {
    var p = UnsafeRawPointer(base)
    var type: Any.Type = Root.self
    var keepAlive: AnyObject?
    
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
      // we don't need the hack of the keepAlive reference to manage closing
      // accesses.
      let typedPointer = p.assumingMemoryBound(to: Value.self)
      return (pointer: UnsafeMutablePointer(mutating: typedPointer),
              owner: keepAlive)
    }
  }

}

/// A key path that supports reading from and writing to the resulting value
/// with reference semantics.
public class ReferenceWritableKeyPath<
  Root, Value
> : WritableKeyPath<Root, Value> {
  // MARK: Implementation detail

  internal final override class var kind: Kind { return .reference }
  
  internal final override func projectMutableAddress(
    from base: UnsafePointer<Root>
  ) -> (pointer: UnsafeMutablePointer<Value>, owner: AnyObject?) {
    // Since we're a ReferenceWritableKeyPath, we know we don't mutate the base
    // in practice.
    return projectMutableAddress(from: base.pointee)
  }
  
  @usableFromInline
  internal final func projectMutableAddress(from origBase: Root)
      -> (pointer: UnsafeMutablePointer<Value>, owner: AnyObject?) {
    var keepAlive: AnyObject?
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
            return rawComponent.projectReadOnly(
              base, to: NewValue.self, endingWith: Value.self)
              .assumingContinue
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
    
    return (address, keepAlive)
  }
}

// MARK: Implementation details

internal enum KeyPathComponentKind {
  /// The keypath references an externally-defined property or subscript whose
  /// component describes how to interact with the key path.
  case external
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
  internal init(value: Int, isStoredProperty: Bool, isTableOffset: Bool) {
    self.value = value
    self.isStoredProperty = isStoredProperty
    self.isTableOffset = isTableOffset
  }

  internal var value: Int
  internal var isStoredProperty: Bool
  internal var isTableOffset: Bool

  internal static func ==(
    x: ComputedPropertyID, y: ComputedPropertyID
  ) -> Bool {
    return x.value == y.value
      && x.isStoredProperty == y.isStoredProperty
      && x.isTableOffset == x.isTableOffset
  }

  internal func hash(into hasher: inout Hasher) {
    hasher.combine(value)
    hasher.combine(isStoredProperty)
    hasher.combine(isTableOffset)
  }
}

internal struct ComputedArgumentWitnesses {
  internal typealias Destroy = @convention(thin)
    (_ instanceArguments: UnsafeMutableRawPointer, _ size: Int) -> ()
  internal typealias Copy = @convention(thin)
    (_ srcInstanceArguments: UnsafeRawPointer,
     _ destInstanceArguments: UnsafeMutableRawPointer,
     _ size: Int) -> ()
  internal typealias Equals = @convention(thin)
    (_ xInstanceArguments: UnsafeRawPointer,
     _ yInstanceArguments: UnsafeRawPointer,
     _ size: Int) -> Bool
  // FIXME(hasher) Combine to an inout Hasher instead
  internal typealias Hash = @convention(thin)
    (_ instanceArguments: UnsafeRawPointer,
     _ size: Int) -> Int

  internal let destroy: Destroy?
  internal let copy: Copy
  internal let equals: Equals
  internal let hash: Hash
}

internal enum KeyPathComponent: Hashable {
  internal struct ArgumentRef {
    internal init(
      data: UnsafeRawBufferPointer,
      witnesses: UnsafePointer<ComputedArgumentWitnesses>,
      witnessSizeAdjustment: Int
    ) {
      self.data = data
      self.witnesses = witnesses
      self.witnessSizeAdjustment = witnessSizeAdjustment
    }

    internal var data: UnsafeRawBufferPointer
    internal var witnesses: UnsafePointer<ComputedArgumentWitnesses>
    internal var witnessSizeAdjustment: Int
  }

  /// The keypath projects within the storage of the outer value, like a
  /// stored property in a struct.
  case `struct`(offset: Int)
  /// The keypath projects from the referenced pointer, like a
  /// stored property in a class.
  case `class`(offset: Int)
  /// The keypath projects using a getter.
  case get(id: ComputedPropertyID,
           get: UnsafeRawPointer, argument: ArgumentRef?)
  /// The keypath projects using a getter/setter pair. The setter can mutate
  /// the base value in-place.
  case mutatingGetSet(id: ComputedPropertyID,
                      get: UnsafeRawPointer, set: UnsafeRawPointer,
                      argument: ArgumentRef?)
  /// The keypath projects using a getter/setter pair that does not mutate its
  /// base.
  case nonmutatingGetSet(id: ComputedPropertyID,
                         get: UnsafeRawPointer, set: UnsafeRawPointer,
                         argument: ArgumentRef?)
  /// The keypath optional-chains, returning nil immediately if the input is
  /// nil, or else proceeding by projecting the value inside.
  case optionalChain
  /// The keypath optional-forces, trapping if the input is
  /// nil, or else proceeding by projecting the value inside.
  case optionalForce
  /// The keypath wraps a value in an optional.
  case optionalWrap

  internal static func ==(a: KeyPathComponent, b: KeyPathComponent) -> Bool {
    switch (a, b) {
    case (.struct(offset: let a), .struct(offset: let b)),
         (.class (offset: let a), .class (offset: let b)):
      return a == b
    case (.optionalChain, .optionalChain),
         (.optionalForce, .optionalForce),
         (.optionalWrap, .optionalWrap):
      return true
    case (.get(id: let id1, get: _, argument: let argument1),
          .get(id: let id2, get: _, argument: let argument2)),

         (.mutatingGetSet(id: let id1, get: _, set: _, argument: let argument1),
          .mutatingGetSet(id: let id2, get: _, set: _, argument: let argument2)),

         (.nonmutatingGetSet(id: let id1, get: _, set: _, argument: let argument1),
          .nonmutatingGetSet(id: let id2, get: _, set: _, argument: let argument2)):
      if id1 != id2 {
        return false
      }
      if let arg1 = argument1, let arg2 = argument2 {
        return arg1.witnesses.pointee.equals(
          arg1.data.baseAddress.unsafelyUnwrapped,
          arg2.data.baseAddress.unsafelyUnwrapped,
          arg1.data.count - arg1.witnessSizeAdjustment)
      }
      // If only one component has arguments, that should indicate that the
      // only arguments in that component were generic captures and therefore
      // not affecting equality.
      return true
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

  @_effects(releasenone)
  internal func hash(into hasher: inout Hasher) {
    func appendHashFromArgument(
      _ argument: KeyPathComponent.ArgumentRef?
    ) {
      if let argument = argument {
        let hash = argument.witnesses.pointee.hash(
          argument.data.baseAddress.unsafelyUnwrapped,
          argument.data.count - argument.witnessSizeAdjustment)
        // Returning 0 indicates that the arguments should not impact the
        // hash value of the overall key path.
        // FIXME(hasher): hash witness should just mutate hasher directly
        if hash != 0 {
          hasher.combine(hash)
        }
      }
    }
    switch self {
    case .struct(offset: let a):
      hasher.combine(0)
      hasher.combine(a)
    case .class(offset: let b):
      hasher.combine(1)
      hasher.combine(b)
    case .optionalChain:
      hasher.combine(2)
    case .optionalForce:
      hasher.combine(3)
    case .optionalWrap:
      hasher.combine(4)
    case .get(id: let id, get: _, argument: let argument):
      hasher.combine(5)
      hasher.combine(id)
      appendHashFromArgument(argument)
    case .mutatingGetSet(id: let id, get: _, set: _, argument: let argument):
      hasher.combine(6)
      hasher.combine(id)
      appendHashFromArgument(argument)
    case .nonmutatingGetSet(id: let id, get: _, set: _, argument: let argument):
      hasher.combine(7)
      hasher.combine(id)
      appendHashFromArgument(argument)
    }
  }
}

// A class that maintains ownership of another object while a mutable projection
// into it is underway. The lifetime of the instance of this class is also used
// to begin and end exclusive 'modify' access to the projected address.
internal final class ClassHolder<ProjectionType> {

  /// The type of the scratch record passed to the runtime to record
  /// accesses to guarantee exlcusive access.
  internal typealias AccessRecord = Builtin.UnsafeValueBuffer

  internal var previous: AnyObject?
  internal var instance: AnyObject

  internal init(previous: AnyObject?, instance: AnyObject) {
    self.previous = previous
    self.instance = instance
  }

  internal final class func _create(
      previous: AnyObject?,
      instance: AnyObject,
      accessingAddress address: UnsafeRawPointer,
      type: ProjectionType.Type
  ) -> ClassHolder {

    // Tail allocate the UnsafeValueBuffer used as the AccessRecord.
    // This avoids a second heap allocation since there is no source-level way to
    // initialize a Builtin.UnsafeValueBuffer type and thus we cannot have a
    // stored property of that type.
    let holder: ClassHolder = Builtin.allocWithTailElems_1(self,
                                                          1._builtinWordValue,
                                                          AccessRecord.self)

    // Initialize the ClassHolder's instance variables. This is done via
    // withUnsafeMutablePointer(to:) because the instance was just allocated with
    // allocWithTailElems_1 and so we need to make sure to use an initialization
    // rather than an assignment.
    withUnsafeMutablePointer(to: &holder.previous) {
      $0.initialize(to: previous)
    }

    withUnsafeMutablePointer(to: &holder.instance) {
      $0.initialize(to: instance)
    }

    let accessRecordPtr = Builtin.projectTailElems(holder, AccessRecord.self)

    // Begin a 'modify' access to the address. This access is ended in
    // ClassHolder's deinitializer.
    Builtin.beginUnpairedModifyAccess(address._rawValue, accessRecordPtr, type)

    return holder
  }

  deinit {
    let accessRecordPtr = Builtin.projectTailElems(self, AccessRecord.self)

    // Ends the access begun in _create().
    Builtin.endUnpairedAccess(accessRecordPtr)
  }
}

// A class that triggers writeback to a pointer when destroyed.
internal final class MutatingWritebackBuffer<CurValue, NewValue> {
  internal let previous: AnyObject?
  internal let base: UnsafeMutablePointer<CurValue>
  internal let set: @convention(thin) (NewValue, inout CurValue, UnsafeRawPointer, Int) -> ()
  internal let argument: UnsafeRawPointer
  internal let argumentSize: Int
  internal var value: NewValue

  deinit {
    set(value, &base.pointee, argument, argumentSize)
  }

  internal init(previous: AnyObject?,
       base: UnsafeMutablePointer<CurValue>,
       set: @escaping @convention(thin) (NewValue, inout CurValue, UnsafeRawPointer, Int) -> (),
       argument: UnsafeRawPointer,
       argumentSize: Int,
       value: NewValue) {
    self.previous = previous
    self.base = base
    self.set = set
    self.argument = argument
    self.argumentSize = argumentSize
    self.value = value
  }
}

// A class that triggers writeback to a non-mutated value when destroyed.
internal final class NonmutatingWritebackBuffer<CurValue, NewValue> {
  internal let previous: AnyObject?
  internal let base: CurValue
  internal let set: @convention(thin) (NewValue, CurValue, UnsafeRawPointer, Int) -> ()
  internal let argument: UnsafeRawPointer
  internal let argumentSize: Int
  internal var value: NewValue

  deinit {
    set(value, base, argument, argumentSize)
  }

  internal
  init(previous: AnyObject?,
       base: CurValue,
       set: @escaping @convention(thin) (NewValue, CurValue, UnsafeRawPointer, Int) -> (),
       argument: UnsafeRawPointer,
       argumentSize: Int,
       value: NewValue) {
    self.previous = previous
    self.base = base
    self.set = set
    self.argument = argument
    self.argumentSize = argumentSize
    self.value = value
  }
}

internal struct RawKeyPathComponent {
  internal init(header: Header, body: UnsafeRawBufferPointer) {
    self.header = header
    self.body = body
  }

  internal var header: Header
  internal var body: UnsafeRawBufferPointer
  
  internal struct Header {
    internal static var payloadMask: UInt32 {
      return _SwiftKeyPathComponentHeader_PayloadMask
    }
    internal static var discriminatorMask: UInt32 {
      return _SwiftKeyPathComponentHeader_DiscriminatorMask
    }
    internal static var discriminatorShift: UInt32 {
      return _SwiftKeyPathComponentHeader_DiscriminatorShift
    }
    internal static var externalTag: UInt32 {
      return _SwiftKeyPathComponentHeader_ExternalTag
    }
    internal static var structTag: UInt32 {
      return _SwiftKeyPathComponentHeader_StructTag
    }
    internal static var computedTag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedTag
    }
    internal static var classTag: UInt32 {
      return _SwiftKeyPathComponentHeader_ClassTag
    }
    internal static var optionalTag: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalTag
    }
    internal static var optionalChainPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalChainPayload
    }
    internal static var optionalWrapPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalWrapPayload
    }
    internal static var optionalForcePayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalForcePayload
    }

    internal static var endOfReferencePrefixFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_EndOfReferencePrefixFlag
    }
    internal static var outOfLineOffsetPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OutOfLineOffsetPayload
    }
    internal static var unresolvedFieldOffsetPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_UnresolvedFieldOffsetPayload
    }
    internal static var unresolvedIndirectOffsetPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_UnresolvedIndirectOffsetPayload
    }
    internal static var maximumOffsetPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_MaximumOffsetPayload
    }

    internal static var computedMutatingFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedMutatingFlag
    }
    internal var isComputedMutating: Bool {
      _sanityCheck(kind == .computed)
      return _value & Header.computedMutatingFlag != 0
    }

    internal static var computedSettableFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedSettableFlag
    }
    internal var isComputedSettable: Bool {
      _sanityCheck(kind == .computed)
      return _value & Header.computedSettableFlag != 0
    }

    internal static var computedIDByStoredPropertyFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDByStoredPropertyFlag
    }
    internal static var computedIDByVTableOffsetFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDByVTableOffsetFlag
    }

    internal static var computedHasArgumentsFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedHasArgumentsFlag
    }
    internal var hasComputedArguments: Bool {
      _sanityCheck(kind == .computed)
      return _value & Header.computedHasArgumentsFlag != 0
    }

    // If a computed component is instantiated from an external property
    // descriptor, and both components carry arguments, we need to carry some
    // extra matter to be able to map between the client and external generic
    // contexts.
    internal static var computedInstantiatedFromExternalWithArgumentsFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedInstantiatedFromExternalWithArgumentsFlag
    }
    internal var isComputedInstantiatedFromExternalWithArguments: Bool {
      get {
        _sanityCheck(kind == .computed)
        return
          _value & Header.computedInstantiatedFromExternalWithArgumentsFlag != 0
      }
      set {
        _sanityCheck(kind == .computed)
        _value =
            _value & ~Header.computedInstantiatedFromExternalWithArgumentsFlag
          | (newValue ? Header.computedInstantiatedFromExternalWithArgumentsFlag
                      : 0)
      }
    }
    internal static var externalWithArgumentsExtraSize: Int {
      return MemoryLayout<Int>.size
    }

    internal static var computedIDResolutionMask: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDResolutionMask
    }
    internal static var computedIDResolved: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDResolved
    }
    internal static var computedIDUnresolvedIndirectPointer: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDUnresolvedIndirectPointer
    }
    
    internal var _value: UInt32
    
    internal var discriminator: UInt32 {
      return (_value & Header.discriminatorMask) >> Header.discriminatorShift
    }
    internal var payload: UInt32 {
      get {
        return _value & Header.payloadMask
      }
      set {
        _sanityCheck(newValue & Header.payloadMask == newValue,
                     "payload too big")
        _value = _value & ~Header.payloadMask | newValue
      }
    }
    internal var endOfReferencePrefix: Bool {
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

    internal var kind: KeyPathComponentKind {
      switch (discriminator, payload) {
      case (Header.externalTag, _):
        return .external
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

    // The component header is 4 bytes, but may be followed by an aligned
    // pointer field for some kinds of component, forcing padding.
    internal static var pointerAlignmentSkew: Int {
      return MemoryLayout<Int>.size - MemoryLayout<Int32>.size
    }

    internal var isTrivialPropertyDescriptor: Bool {
      return _value ==
        _SwiftKeyPathComponentHeader_TrivialPropertyDescriptorMarker
    }

    /// If this is the header for a component in a key path pattern, return
    /// the size of the body of the component.
    internal var patternComponentBodySize: Int {
      return _componentBodySize(forPropertyDescriptor: false)
    }

    /// If this is the header for a property descriptor, return
    /// the size of the body of the component.
    internal var propertyDescriptorBodySize: Int {
      if isTrivialPropertyDescriptor { return 0 }
      return _componentBodySize(forPropertyDescriptor: true)
    }

    internal func _componentBodySize(forPropertyDescriptor: Bool) -> Int {
      switch kind {
      case .struct, .class:
        if payload == Header.unresolvedFieldOffsetPayload
           || payload == Header.outOfLineOffsetPayload {
          // A 32-bit offset is stored in the body.
          return MemoryLayout<UInt32>.size
        }
        if payload == Header.unresolvedIndirectOffsetPayload {
          // A pointer-aligned, pointer-sized pointer is stored in the body.
          return Header.pointerAlignmentSkew + MemoryLayout<Int>.size
        }
        // Otherwise, there's no body.
        return Header.pointerAlignmentSkew

      case .external:
        // The body holds a pointer to the external property descriptor,
        // and some number of substitution arguments, the count of which is
        // in the payload.
        return Header.pointerAlignmentSkew
          + MemoryLayout<Int>.size * (1 + Int(payload))

      case .computed:
        // The body holds at minimum the id and getter.
        var size = Header.pointerAlignmentSkew + MemoryLayout<Int>.size * 2
        // If settable, it also holds the setter.
        if isComputedSettable {
          size += MemoryLayout<Int>.size
        }
        // If there are arguments, there's also a layout function,
        // witness table, and initializer function.
        // Property descriptors never carry argument information, though.
        if !forPropertyDescriptor && hasComputedArguments {
          size += MemoryLayout<Int>.size * 3
        }

        return size

      case .optionalForce, .optionalChain, .optionalWrap:
        // Otherwise, there's no body.
        return Header.pointerAlignmentSkew
      }
    }
  }

  internal var bodySize: Int {
    let ptrSize = MemoryLayout<Int>.size
    switch header.kind {
    case .struct, .class:
      if header.payload == Header.payloadMask { return 4 } // overflowed
      return 0
    case .external:
      // align to pointer + pointer to external descriptor
      // + N generic argument accessors (N in payload)
      return Header.pointerAlignmentSkew + ptrSize * (1 + Int(header.payload))
    case .optionalChain, .optionalForce, .optionalWrap:
      return 0
    case .computed:
      // align to pointer, minimum two pointers for id and get
      var total = Header.pointerAlignmentSkew + ptrSize * 2
      // additional word for a setter
      if header.isComputedSettable {
        total += ptrSize
      }
      // include the argument size
      if header.hasComputedArguments {
        // two words for argument header: size, witnesses
        total += ptrSize * 2
        // size of argument area
        total += _computedArgumentSize
        if header.isComputedInstantiatedFromExternalWithArguments {
          total += Header.externalWithArgumentsExtraSize
        }
      }
      return total
    }
  }

  internal var _structOrClassOffset: Int {
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

  internal var _computedIDValue: Int {
    _sanityCheck(header.kind == .computed,
                 "not a computed property")
    return body.load(fromByteOffset: Header.pointerAlignmentSkew,
                     as: Int.self)
  }

  internal var _computedID: ComputedPropertyID {
    let payload = header.payload
    return ComputedPropertyID(
      value: _computedIDValue,
      isStoredProperty: payload & Header.computedIDByStoredPropertyFlag != 0,
      isTableOffset: payload & Header.computedIDByVTableOffsetFlag != 0)
  }

  internal var _computedGetter: UnsafeRawPointer {
    _sanityCheck(header.kind == .computed,
                 "not a computed property")

    return body.load(
      fromByteOffset: Header.pointerAlignmentSkew + MemoryLayout<Int>.size,
      as: UnsafeRawPointer.self)
  }

  internal var _computedSetter: UnsafeRawPointer {
    _sanityCheck(header.isComputedSettable,
                 "not a settable property")

    return body.load(
      fromByteOffset: Header.pointerAlignmentSkew + MemoryLayout<Int>.size * 2,
      as: UnsafeRawPointer.self)
  }

  internal typealias ComputedArgumentLayoutFn = @convention(thin)
    (_ patternArguments: UnsafeRawPointer) -> (size: Int, alignmentMask: Int)
  internal typealias ComputedArgumentInitializerFn = @convention(thin)
    (_ patternArguments: UnsafeRawPointer,
     _ instanceArguments: UnsafeMutableRawPointer) -> ()

  internal var _computedArgumentHeaderPointer: UnsafeRawPointer {
    _sanityCheck(header.hasComputedArguments, "no arguments")

    return body.baseAddress.unsafelyUnwrapped
      + Header.pointerAlignmentSkew
      + MemoryLayout<Int>.size *
         (header.isComputedSettable ? 3 : 2)
  }

  internal var _computedArgumentSize: Int {
    return _computedArgumentHeaderPointer.load(as: Int.self)
  }
  internal
  var _computedArgumentWitnesses: UnsafePointer<ComputedArgumentWitnesses> {
    return _computedArgumentHeaderPointer.load(
      fromByteOffset: MemoryLayout<Int>.size,
      as: UnsafePointer<ComputedArgumentWitnesses>.self)
  }

  internal var _computedArguments: UnsafeRawPointer {
    var base = _computedArgumentHeaderPointer + MemoryLayout<Int>.size * 2
    // If the component was instantiated from an external property descriptor
    // with its own arguments, we include some additional capture info to
    // be able to map to the original argument context by adjusting the size
    // passed to the witness operations.
    if header.isComputedInstantiatedFromExternalWithArguments {
      base += Header.externalWithArgumentsExtraSize
    }
    return base
  }
  internal var _computedMutableArguments: UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(mutating: _computedArguments)
  }
  internal var _computedArgumentWitnessSizeAdjustment: Int {
    if header.isComputedInstantiatedFromExternalWithArguments {
      return _computedArguments.load(
        fromByteOffset: -Header.externalWithArgumentsExtraSize,
        as: Int.self)
    }
    return 0
  }

  internal var value: KeyPathComponent {
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
      let isSettable = header.isComputedSettable
      let isMutating = header.isComputedMutating

      let id = _computedID
      let get = _computedGetter
      // Argument value is unused if there are no arguments.
      let argument: KeyPathComponent.ArgumentRef?
      if header.hasComputedArguments {
        argument = KeyPathComponent.ArgumentRef(
          data: UnsafeRawBufferPointer(start: _computedArguments,
                                       count: _computedArgumentSize),
          witnesses: _computedArgumentWitnesses,
          witnessSizeAdjustment: _computedArgumentWitnessSizeAdjustment)
      } else {
        argument = nil
      }

      switch (isSettable, isMutating) {
      case (false, false):
        return .get(id: id, get: get, argument: argument)
      case (true, false):
        return .nonmutatingGetSet(id: id,
                                  get: get,
                                  set: _computedSetter,
                                  argument: argument)
      case (true, true):
        return .mutatingGetSet(id: id,
                               get: get,
                               set: _computedSetter,
                               argument: argument)
      case (false, true):
        _sanityCheckFailure("impossible")
      }
    case .external:
      _sanityCheckFailure("should have been instantiated away")
    }
  }

  internal func destroy() {
    switch header.kind {
    case .struct,
         .class,
         .optionalChain,
         .optionalForce,
         .optionalWrap:
      // trivial
      break
    case .computed:
      // Run destructor, if any
      if header.hasComputedArguments,
         let destructor = _computedArgumentWitnesses.pointee.destroy {
        destructor(_computedMutableArguments,
                 _computedArgumentSize - _computedArgumentWitnessSizeAdjustment)
      }
    case .external:
      _sanityCheckFailure("should have been instantiated away")
    }
  }

  internal func clone(into buffer: inout UnsafeMutableRawBufferPointer,
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
      // Fields are pointer-aligned after the header
      componentSize += Header.pointerAlignmentSkew
      buffer.storeBytes(of: _computedIDValue,
                        toByteOffset: componentSize,
                        as: Int.self)
      componentSize += MemoryLayout<Int>.size
      buffer.storeBytes(of: _computedGetter,
                        toByteOffset: componentSize,
                        as: UnsafeRawPointer.self)
      componentSize += MemoryLayout<Int>.size


      if header.isComputedSettable {
        buffer.storeBytes(of: _computedSetter,
                          toByteOffset: MemoryLayout<Int>.size * 3,
                          as: UnsafeRawPointer.self)
        componentSize += MemoryLayout<Int>.size
      }

      if header.hasComputedArguments {
        let arguments = _computedArguments
        let argumentSize = _computedArgumentSize
        buffer.storeBytes(of: argumentSize,
                          toByteOffset: componentSize,
                          as: Int.self)
        componentSize += MemoryLayout<Int>.size
        buffer.storeBytes(of: _computedArgumentWitnesses,
                          toByteOffset: componentSize,
                          as: UnsafePointer<ComputedArgumentWitnesses>.self)
        componentSize += MemoryLayout<Int>.size

        if header.isComputedInstantiatedFromExternalWithArguments {
          // Include the extra matter for components instantiated from
          // external property descriptors with arguments.
          buffer.storeBytes(of: _computedArgumentWitnessSizeAdjustment,
                            toByteOffset: componentSize,
                            as: Int.self)
          componentSize += MemoryLayout<Int>.size
        }
        let adjustedSize = argumentSize - _computedArgumentWitnessSizeAdjustment
        let argumentDest =
          buffer.baseAddress.unsafelyUnwrapped + componentSize
        _computedArgumentWitnesses.pointee.copy(
          arguments,
          argumentDest,
          adjustedSize)
        if header.isComputedInstantiatedFromExternalWithArguments {
          // The extra information for external property descriptor arguments
          // can always be memcpy'd.
          _memcpy(dest: argumentDest + adjustedSize,
                  src: arguments + adjustedSize,
                  size: UInt(_computedArgumentWitnessSizeAdjustment))
        }

        componentSize += argumentSize
      }

    case .external:
      _sanityCheckFailure("should have been instantiated away")
    }
    buffer = UnsafeMutableRawBufferPointer(
      start: buffer.baseAddress.unsafelyUnwrapped + componentSize,
      count: buffer.count - componentSize)
  }

  internal enum ProjectionResult<NewValue, LeafValue> {
    /// Continue projecting the key path with the given new value.
    case `continue`(NewValue)
    /// Stop projecting the key path and use the given value as the final
    /// result of the projection.
    case `break`(LeafValue)

    internal var assumingContinue: NewValue {
      switch self {
      case .continue(let x):
        return x
      case .break:
        _sanityCheckFailure("should not have stopped key path projection")
      }
    }
  }

  internal func projectReadOnly<CurValue, NewValue, LeafValue>(
    _ base: CurValue,
    to: NewValue.Type,
    endingWith: LeafValue.Type
  ) -> ProjectionResult<NewValue, LeafValue> {
    switch value {
    case .struct(let offset):
      var base2 = base
      return .continue(withUnsafeBytes(of: &base2) {
        let p = $0.baseAddress.unsafelyUnwrapped.advanced(by: offset)
        // The contents of the struct should be well-typed, so we can assume
        // typed memory here.
        return p.assumingMemoryBound(to: NewValue.self).pointee
      })

    case .class(let offset):
      _sanityCheck(CurValue.self is AnyObject.Type,
                   "base is not a class")
      let baseObj = unsafeBitCast(base, to: AnyObject.self)
      let basePtr = UnsafeRawPointer(Builtin.bridgeToRawPointer(baseObj))
      defer { _fixLifetime(baseObj) }

      let offsetAddress = basePtr.advanced(by: offset)

      // Perform an instaneous record access on the address in order to
      // ensure that the read will not conflict with an already in-progress
      // 'modify' access.
      Builtin.performInstantaneousReadAccess(offsetAddress._rawValue,
        NewValue.self)
      return .continue(offsetAddress
        .assumingMemoryBound(to: NewValue.self)
        .pointee)

    case .get(id: _, get: let rawGet, argument: let argument),
         .mutatingGetSet(id: _, get: let rawGet, set: _, argument: let argument),
         .nonmutatingGetSet(id: _, get: let rawGet, set: _, argument: let argument):
      typealias Getter
        = @convention(thin) (CurValue, UnsafeRawPointer, Int) -> NewValue
      let get = unsafeBitCast(rawGet, to: Getter.self)
      return .continue(get(base,
                           argument?.data.baseAddress ?? rawGet,
                           argument?.data.count ?? 0))

    case .optionalChain:
      _sanityCheck(CurValue.self == Optional<NewValue>.self,
                   "should be unwrapping optional value")
      _sanityCheck(_isOptional(LeafValue.self),
                   "leaf result should be optional")
      if let baseValue = unsafeBitCast(base, to: Optional<NewValue>.self) {
        return .continue(baseValue)
      } else {
        // TODO: A more efficient way of getting the `none` representation
        // of a dynamically-optional type...
        return .break((Optional<()>.none as Any) as! LeafValue)
      }

    case .optionalForce:
      _sanityCheck(CurValue.self == Optional<NewValue>.self,
                   "should be unwrapping optional value")
      return .continue(unsafeBitCast(base, to: Optional<NewValue>.self)!)

    case .optionalWrap:
      _sanityCheck(NewValue.self == Optional<CurValue>.self,
                   "should be wrapping optional value")
      return .continue(
        unsafeBitCast(base as Optional<CurValue>, to: NewValue.self))
    }
  }

  internal func projectMutableAddress<CurValue, NewValue>(
    _ base: UnsafeRawPointer,
    from _: CurValue.Type,
    to _: NewValue.Type,
    isRoot: Bool,
    keepAlive: inout AnyObject?
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
      let offsetAddress = UnsafeRawPointer(Builtin.bridgeToRawPointer(object))
            .advanced(by: offset)

      // Keep the  base alive for the duration of the derived access and also
      // enforce exclusive access to the address.
      keepAlive = ClassHolder._create(previous: keepAlive, instance: object,
                                      accessingAddress: offsetAddress,
                                      type: NewValue.self)

      return offsetAddress
    
    case .mutatingGetSet(id: _, get: let rawGet, set: let rawSet,
                         argument: let argument):
      typealias Getter
        = @convention(thin) (CurValue, UnsafeRawPointer, Int) -> NewValue
      typealias Setter
        = @convention(thin) (NewValue, inout CurValue, UnsafeRawPointer, Int) -> ()
      let get = unsafeBitCast(rawGet, to: Getter.self)
      let set = unsafeBitCast(rawSet, to: Setter.self)

      let baseTyped = UnsafeMutablePointer(
        mutating: base.assumingMemoryBound(to: CurValue.self))

      let argValue = argument?.data.baseAddress ?? rawGet
      let argSize = argument?.data.count ?? 0
      let writeback = MutatingWritebackBuffer(previous: keepAlive,
                               base: baseTyped,
                               set: set,
                               argument: argValue,
                               argumentSize: argSize,
                               value: get(baseTyped.pointee, argValue, argSize))
      keepAlive = writeback
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
        = @convention(thin) (CurValue, UnsafeRawPointer, Int) -> NewValue
      typealias Setter
        = @convention(thin) (NewValue, CurValue, UnsafeRawPointer, Int) -> ()

      let get = unsafeBitCast(rawGet, to: Getter.self)
      let set = unsafeBitCast(rawSet, to: Setter.self)

      let baseValue = base.assumingMemoryBound(to: CurValue.self).pointee
      let argValue = argument?.data.baseAddress ?? rawGet
      let argSize = argument?.data.count ?? 0
      let writeback = NonmutatingWritebackBuffer(previous: keepAlive,
                                       base: baseValue,
                                       set: set,
                                       argument: argValue,
                                       argumentSize: argSize,
                                       value: get(baseValue, argValue, argSize))
      keepAlive = writeback
      // A maximally-abstracted, final, stored class property should have
      // a stable address.
      return UnsafeRawPointer(Builtin.addressof(&writeback.value))

    case .optionalForce:
      _sanityCheck(CurValue.self == Optional<NewValue>.self,
                   "should be unwrapping an optional value")
      // Optional's layout happens to always put the payload at the start
      // address of the Optional value itself, if a value is present at all.
      let baseOptionalPointer
        = base.assumingMemoryBound(to: Optional<NewValue>.self)
      // Assert that a value exists
      _ = baseOptionalPointer.pointee!
      return base
    
    case .optionalChain, .optionalWrap, .get:
      _sanityCheckFailure("not a mutable key path component")
    }
  }
}

internal struct KeyPathBuffer {
  internal var data: UnsafeRawBufferPointer
  internal var trivial: Bool
  internal var hasReferencePrefix: Bool

  internal var mutableData: UnsafeMutableRawBufferPointer {
    return UnsafeMutableRawBufferPointer(mutating: data)
  }

  internal struct Header {
    internal var _value: UInt32
    
    internal static var sizeMask: UInt32 {
      return _SwiftKeyPathBufferHeader_SizeMask
    }
    internal static var reservedMask: UInt32 {
      return _SwiftKeyPathBufferHeader_ReservedMask
    }
    internal static var trivialFlag: UInt32 {
      return _SwiftKeyPathBufferHeader_TrivialFlag
    }
    internal static var hasReferencePrefixFlag: UInt32 {
      return _SwiftKeyPathBufferHeader_HasReferencePrefixFlag
    }

    internal init(size: Int, trivial: Bool, hasReferencePrefix: Bool) {
      _sanityCheck(size <= Int(Header.sizeMask), "key path too big")
      _value = UInt32(size)
        | (trivial ? Header.trivialFlag : 0)
        | (hasReferencePrefix ? Header.hasReferencePrefixFlag : 0)
    }

    internal var size: Int { return Int(_value & Header.sizeMask) }
    internal var trivial: Bool { return _value & Header.trivialFlag != 0 }
    internal var hasReferencePrefix: Bool {
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
    internal var instantiableInLine: Bool {
      return trivial
    }

    internal func validateReservedBits() {
      _precondition(_value & Header.reservedMask == 0,
                    "Reserved bits set to an unexpected bit pattern")
    }
  }

  internal init(base: UnsafeRawPointer) {
    let header = base.load(as: Header.self)
    data = UnsafeRawBufferPointer(
      start: base + MemoryLayout<Int>.size,
      count: header.size)
    trivial = header.trivial
    hasReferencePrefix = header.hasReferencePrefix
  }

  internal init(partialData: UnsafeRawBufferPointer,
                trivial: Bool = false,
                hasReferencePrefix: Bool = false) {
    self.data = partialData
    self.trivial = trivial
    self.hasReferencePrefix = hasReferencePrefix
  }
  
  internal func destroy() {
    // Short-circuit if nothing in the object requires destruction.
    if trivial { return }
    
    var bufferToDestroy = self
    while true {
      let (component, type) = bufferToDestroy.next()
      component.destroy()
      guard let _ = type else { break }
    }
  }
  
  internal mutating func next() -> (RawKeyPathComponent, Any.Type?) {
    let header = pop(RawKeyPathComponent.Header.self)
    // Track if this is the last component of the reference prefix.
    if header.endOfReferencePrefix {
      _sanityCheck(self.hasReferencePrefix,
                   "beginMutation marker in non-reference-writable key path?")
      self.hasReferencePrefix = false
    }
    
    var component = RawKeyPathComponent(header: header, body: data)
    // Shrinkwrap the component buffer size.
    let size = component.bodySize
    component.body = UnsafeRawBufferPointer(start: component.body.baseAddress,
                                            count: size)
    _ = popRaw(size: size, alignment: Int8.self)

    // fetch type, which is in the buffer unless it's the final component
    let nextType: Any.Type?
    if data.count == 0 {
      nextType = nil
    } else {
      nextType = pop(Any.Type.self)
    }
    return (component, nextType)
  }
  
  internal mutating func pop<T>(_ type: T.Type) -> T {
    _sanityCheck(_isPOD(T.self), "should be POD")
    let raw = popRaw(size: MemoryLayout<T>.size,
                     alignment: T.self)
    let resultBuf = UnsafeMutablePointer<T>.allocate(capacity: 1)
    _memcpy(dest: resultBuf,
            src: raw.baseAddress.unsafelyUnwrapped,
            size: UInt(MemoryLayout<T>.size))
    let result = resultBuf.pointee
    resultBuf.deallocate()
    return result
  }
  internal mutating func popRaw<Alignment>(
    size: Int, alignment: Alignment.Type
  ) -> UnsafeRawBufferPointer {
    data = MemoryLayout<Alignment>._roundingUpBaseToAlignment(data)
    let result = UnsafeRawBufferPointer(start: data.baseAddress, count: size)
    data = UnsafeRawBufferPointer(
      start: data.baseAddress.unsafelyUnwrapped + size,
      count: data.count - size)
    return result
  }
}

// MARK: Library intrinsics for projecting key paths.

@inlinable
public // COMPILER_INTRINSIC
func _projectKeyPathPartial<Root>(
  root: Root,
  keyPath: PartialKeyPath<Root>
) -> Any {
  func open<Value>(_: Value.Type) -> Any {
    return _projectKeyPathReadOnly(root: root,
      keyPath: unsafeDowncast(keyPath, to: KeyPath<Root, Value>.self))
  }
  return _openExistential(type(of: keyPath).valueType, do: open)
}

@inlinable
public // COMPILER_INTRINSIC
func _projectKeyPathAny<RootValue>(
  root: RootValue,
  keyPath: AnyKeyPath
) -> Any? {
  let (keyPathRoot, keyPathValue) = type(of: keyPath)._rootAndValueType
  func openRoot<KeyPathRoot>(_: KeyPathRoot.Type) -> Any? {
    guard let rootForKeyPath = root as? KeyPathRoot else {
      return nil
    }
    func openValue<Value>(_: Value.Type) -> Any {
      return _projectKeyPathReadOnly(root: rootForKeyPath,
        keyPath: unsafeDowncast(keyPath, to: KeyPath<KeyPathRoot, Value>.self))
    }
    return _openExistential(keyPathValue, do: openValue)
  }
  return _openExistential(keyPathRoot, do: openRoot)
}

@inlinable
public // COMPILER_INTRINSIC
func _projectKeyPathReadOnly<Root, Value>(
  root: Root,
  keyPath: KeyPath<Root, Value>
) -> Value {
  return keyPath.projectReadOnly(from: root)
}

@inlinable
public // COMPILER_INTRINSIC
func _projectKeyPathWritable<Root, Value>(
  root: UnsafeMutablePointer<Root>,
  keyPath: WritableKeyPath<Root, Value>
) -> (UnsafeMutablePointer<Value>, AnyObject?) {
  return keyPath.projectMutableAddress(from: root)
}

@inlinable
public // COMPILER_INTRINSIC
func _projectKeyPathReferenceWritable<Root, Value>(
  root: Root,
  keyPath: ReferenceWritableKeyPath<Root, Value>
) -> (UnsafeMutablePointer<Value>, AnyObject?) {
  return keyPath.projectMutableAddress(from: root)
}

// MARK: Appending type system

// FIXME(ABI): The type relationships between KeyPath append operands are tricky
// and don't interact well with our overriding rules. Hack things by injecting
// a bunch of `appending` overloads as protocol extensions so they aren't
// constrained by being overrides, and so that we can use exact-type constraints
// on `Self` to prevent dynamically-typed methods from being inherited by
// statically-typed key paths.

/// An implementation detail of key path expressions; do not use this protocol
/// directly.
@_show_in_interface
public protocol _AppendKeyPath {}

extension _AppendKeyPath where Self == AnyKeyPath {
  /// Returns a new key path created by appending the given key path to this
  /// one.
  ///
  /// Use this method to extend this key path to the value type of another key
  /// path. Appending the key path passed as `path` is successful only if the
  /// root type for `path` matches this key path's value type. This example
  /// creates key paths from `Array<Int>` to `String` and from `String` to
  /// `Int`, and then tries appending each to the other:
  ///
  ///     let arrayDescription: AnyKeyPath = \Array<Int>.description
  ///     let stringLength: AnyKeyPath = \String.count
  ///
  ///     // Creates a key path from `Array<Int>` to `Int`
  ///     let arrayDescriptionLength = arrayDescription.appending(path: stringLength)
  ///
  ///     let invalidKeyPath = stringLength.appending(path: arrayDescription)
  ///     // invalidKeyPath == nil
  ///
  /// The second call to `appending(path:)` returns `nil`
  /// because the root type of `arrayDescription`, `Array<Int>`, does not
  /// match the value type of `stringLength`, `Int`.
  ///
  /// - Parameter path: The key path to append.
  /// - Returns: A key path from the root of this key path and the value type
  ///   of `path`, if `path` can be appended. If `path` can't be appended,
  ///   returns `nil`.
  @inlinable
  public func appending(path: AnyKeyPath) -> AnyKeyPath? {
    return _tryToAppendKeyPaths(root: self, leaf: path)
  }
}

extension _AppendKeyPath /* where Self == PartialKeyPath<T> */ {
  /// Returns a new key path created by appending the given key path to this
  /// one.
  ///
  /// Use this method to extend this key path to the value type of another key
  /// path. Appending the key path passed as `path` is successful only if the
  /// root type for `path` matches this key path's value type. This example
  /// creates key paths from `Array<Int>` to `String` and from `String` to
  /// `Int`, and then tries appending each to the other:
  ///
  ///     let arrayDescription: PartialKeyPath<Array<Int>> = \.description
  ///     let stringLength: PartialKeyPath<String> = \.count
  ///
  ///     // Creates a key path from `Array<Int>` to `Int`
  ///     let arrayDescriptionLength = arrayDescription.appending(path: stringLength)
  ///
  ///     let invalidKeyPath = stringLength.appending(path: arrayDescription)
  ///     // invalidKeyPath == nil
  ///
  /// The second call to `appending(path:)` returns `nil`
  /// because the root type of `arrayDescription`, `Array<Int>`, does not
  /// match the value type of `stringLength`, `Int`.
  ///
  /// - Parameter path: The key path to append.
  /// - Returns: A key path from the root of this key path and the value type
  ///   of `path`, if `path` can be appended. If `path` can't be appended,
  ///   returns `nil`.
  @inlinable
  public func appending<Root>(path: AnyKeyPath) -> PartialKeyPath<Root>?
  where Self == PartialKeyPath<Root> {
    return _tryToAppendKeyPaths(root: self, leaf: path)
  }
  
  /// Returns a new key path created by appending the given key path to this
  /// one.
  ///
  /// Use this method to extend this key path to the value type of another key
  /// path. Appending the key path passed as `path` is successful only if the
  /// root type for `path` matches this key path's value type. This example
  /// creates a key path from `Array<Int>` to `String`, and then tries
  /// appending compatible and incompatible key paths:
  ///
  ///     let arrayDescription: PartialKeyPath<Array<Int>> = \.description
  ///
  ///     // Creates a key path from `Array<Int>` to `Int`
  ///     let arrayDescriptionLength = arrayDescription.appending(path: \String.count)
  ///
  ///     let invalidKeyPath = arrayDescription.appending(path: \Double.isZero)
  ///     // invalidKeyPath == nil
  ///
  /// The second call to `appending(path:)` returns `nil` because the root type
  /// of the `path` parameter, `Double`, does not match the value type of
  /// `arrayDescription`, `String`.
  ///
  /// - Parameter path: The key path to append.
  /// - Returns: A key path from the root of this key path to the value type
  ///   of `path`, if `path` can be appended. If `path` can't be appended,
  ///   returns `nil`.
  @inlinable
  public func appending<Root, AppendedRoot, AppendedValue>(
    path: KeyPath<AppendedRoot, AppendedValue>
  ) -> KeyPath<Root, AppendedValue>?
  where Self == PartialKeyPath<Root> {
    return _tryToAppendKeyPaths(root: self, leaf: path)
  }
  
  /// Returns a new key path created by appending the given key path to this
  /// one.
  ///
  /// Use this method to extend this key path to the value type of another key
  /// path. Appending the key path passed as `path` is successful only if the
  /// root type for `path` matches this key path's value type.
  ///
  /// - Parameter path: The reference writeable key path to append.
  /// - Returns: A key path from the root of this key path to the value type
  ///   of `path`, if `path` can be appended. If `path` can't be appended,
  ///   returns `nil`.
  @inlinable
  public func appending<Root, AppendedRoot, AppendedValue>(
    path: ReferenceWritableKeyPath<AppendedRoot, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>?
  where Self == PartialKeyPath<Root> {
    return _tryToAppendKeyPaths(root: self, leaf: path)
  }
}

extension _AppendKeyPath /* where Self == KeyPath<T,U> */ {
  /// Returns a new key path created by appending the given key path to this
  /// one.
  ///
  /// Use this method to extend this key path to the value type of another key
  /// path. Calling `appending(path:)` results in the same key path as if the
  /// given key path had been specified using dot notation. In the following
  /// example, `keyPath1` and `keyPath2` are equivalent:
  ///
  ///     let arrayDescription = \Array<Int>.description
  ///     let keyPath1 = arrayDescription.appending(path: \String.count)
  ///
  ///     let keyPath2 = \Array<Int>.description.count
  ///
  /// - Parameter path: The key path to append.
  /// - Returns: A key path from the root of this key path to the value type of
  ///   `path`.
  @inlinable
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

  /// Returns a new key path created by appending the given key path to this
  /// one.
  ///
  /// Use this method to extend this key path to the value type of another key
  /// path. Calling `appending(path:)` results in the same key path as if the
  /// given key path had been specified using dot notation.
  ///
  /// - Parameter path: The key path to append.
  /// - Returns: A key path from the root of this key path to the value type of
  ///   `path`.
  @inlinable
  public func appending<Root, Value, AppendedValue>(
    path: ReferenceWritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>
  where Self == KeyPath<Root, Value> {
    return _appendingKeyPaths(root: self, leaf: path)
  }
}

extension _AppendKeyPath /* where Self == WritableKeyPath<T,U> */ {
  /// Returns a new key path created by appending the given key path to this
  /// one.
  ///
  /// Use this method to extend this key path to the value type of another key
  /// path. Calling `appending(path:)` results in the same key path as if the
  /// given key path had been specified using dot notation.
  ///
  /// - Parameter path: The key path to append.
  /// - Returns: A key path from the root of this key path to the value type of
  ///   `path`.
  @inlinable
  public func appending<Root, Value, AppendedValue>(
    path: WritableKeyPath<Value, AppendedValue>
  ) -> WritableKeyPath<Root, AppendedValue>
  where Self == WritableKeyPath<Root, Value> {
    return _appendingKeyPaths(root: self, leaf: path)
  }

  /// Returns a new key path created by appending the given key path to this
  /// one.
  ///
  /// Use this method to extend this key path to the value type of another key
  /// path. Calling `appending(path:)` results in the same key path as if the
  /// given key path had been specified using dot notation.
  ///
  /// - Parameter path: The key path to append.
  /// - Returns: A key path from the root of this key path to the value type of
  ///   `path`.
  @inlinable
  public func appending<Root, Value, AppendedValue>(
    path: ReferenceWritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>
  where Self == WritableKeyPath<Root, Value> {
    return _appendingKeyPaths(root: self, leaf: path)
  }
}

extension _AppendKeyPath /* where Self == ReferenceWritableKeyPath<T,U> */ {
  /// Returns a new key path created by appending the given key path to this
  /// one.
  ///
  /// Use this method to extend this key path to the value type of another key
  /// path. Calling `appending(path:)` results in the same key path as if the
  /// given key path had been specified using dot notation.
  ///
  /// - Parameter path: The key path to append.
  /// - Returns: A key path from the root of this key path to the value type of
  ///   `path`.
  @inlinable
  public func appending<Root, Value, AppendedValue>(
    path: WritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>
  where Self == ReferenceWritableKeyPath<Root, Value> {
    return _appendingKeyPaths(root: self, leaf: path)
  }
}

@usableFromInline
internal func _tryToAppendKeyPaths<Result: AnyKeyPath>(
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

@usableFromInline
internal func _appendingKeyPaths<
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
        rootKVCLength = Int(_stdlib_strlen(rootPtr))
        leafKVCLength = Int(_stdlib_strlen(leafPtr))
        // root + "." + leaf
        appendedKVCLength = rootKVCLength + 1 + leafKVCLength
      } else {
        rootKVCLength = 0
        leafKVCLength = 0
        appendedKVCLength = 0
      }

      // Result buffer has room for both key paths' components, plus the
      // header, plus space for the middle type.
      // Align up the root so that we can put the component type after it.
      let rootSize = MemoryLayout<Int>._roundingUpToAlignment(rootBuffer.data.count)
      let resultSize = rootSize + leafBuffer.data.count
        + 2 * MemoryLayout<Int>.size
      // Tail-allocate space for the KVC string.
      let totalResultSize = MemoryLayout<Int32>
        ._roundingUpToAlignment(resultSize + appendedKVCLength)

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
        
        func pushRaw(size: Int, alignment: Int)
            -> UnsafeMutableRawBufferPointer {
          var baseAddress = destBuffer.baseAddress.unsafelyUnwrapped
          var misalign = Int(bitPattern: baseAddress) % alignment
          if misalign != 0 {
            misalign = alignment - misalign
            baseAddress = baseAddress.advanced(by: misalign)
          }
          let result = UnsafeMutableRawBufferPointer(
            start: baseAddress,
            count: size)
          destBuffer = UnsafeMutableRawBufferPointer(
            start: baseAddress + size,
            count: destBuffer.count - size - misalign)
          return result
        }
        func push<T>(_ value: T) {
          let buf = pushRaw(size: MemoryLayout<T>.size,
                            alignment: MemoryLayout<T>.alignment)
          buf.storeBytes(of: value, as: T.self)
        }
        
        // Save space for the header.
        let leafIsReferenceWritable = type(of: leaf).kind == .reference
        let header = KeyPathBuffer.Header(
          size: resultSize - MemoryLayout<Int>.size,
          trivial: rootBuffer.trivial && leafBuffer.trivial,
          hasReferencePrefix: rootBuffer.hasReferencePrefix
                              || leafIsReferenceWritable
        )
        push(header)
        // Start the components at pointer alignment
        _ = pushRaw(size: RawKeyPathComponent.Header.pointerAlignmentSkew,
                alignment: 4)
        
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
            endOfReferencePrefix: endOfReferencePrefix)
          if let type = type {
            push(type)
          } else {
            // Insert our endpoint type between the root and leaf components.
            push(Value.self as Any.Type)
            break
          }
        }
        
        // Clone the leaf components into the buffer.
        while true {
          let (component, type) = leafBuffer.next()

          component.clone(
            into: &destBuffer,
            endOfReferencePrefix: component.header.endOfReferencePrefix)

          if let type = type {
            push(type)
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
                src: rootPtr,
                size: UInt(rootKVCLength))
        kvcStringBuffer.advanced(by: rootKVCLength)
          .storeBytes(of: 0x2E /* '.' */, as: CChar.self)
        _memcpy(dest: kvcStringBuffer.advanced(by: rootKVCLength + 1),
                src: leafPtr,
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
  // initialize it. (The resulting object will still have the
  // "trivial" bit set, since a global object never needs destruction.)
  let bufferHeader = bufferPtr.load(as: KeyPathBuffer.Header.self)
  bufferHeader.validateReservedBits()

  if bufferHeader.instantiableInLine {
    Builtin.onceWithContext(oncePtr._rawValue, _getKeyPath_instantiateInline,
                            patternPtr._rawValue)

    // Return the instantiated object at +1.
    let objectPtr: UnsafeRawPointer

    // If in-place instantiation failed, then the first word of the pattern
    // buffer will be null, and the second word will contain the out-of-line
    // object that was instantiated instead.
    let firstWord = patternPtr.load(as: UnsafeRawPointer?.self)
    if firstWord == nil {
      objectPtr = patternPtr.load(fromByteOffset: MemoryLayout<Int>.size,
                                  as: UnsafeRawPointer.self)
    } else {
      objectPtr = UnsafeRawPointer(patternPtr)
    }

    let object = Unmanaged<AnyKeyPath>.fromOpaque(objectPtr)
    // TODO: This retain will be unnecessary once we support global objects
    // with inert refcounting.
    _ = object.retain()
    return objectPtr
  }

  // Otherwise, instantiate a new key path object modeled on the pattern.
  // Do a pass to determine the class of the key path we'll be instantiating
  // and how much space we'll need for it.
  let (keyPathClass, rootType, size, alignmentMask)
    = _getKeyPathClassAndInstanceSizeFromPattern(patternPtr, arguments)
  return _getKeyPath_instantiateOutOfLine(
    pattern: patternPtr,
    arguments: arguments,
    keyPathClass: keyPathClass,
    rootType: rootType,
    size: size,
    alignmentMask: alignmentMask)
}

internal func _getKeyPath_instantiateOutOfLine(
  pattern: UnsafeRawPointer,
  arguments: UnsafeRawPointer,
  keyPathClass: AnyKeyPath.Type,
  rootType: Any.Type,
  size: Int,
  alignmentMask: Int)
    -> UnsafeRawPointer {
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
  let (keyPathClass, rootType, instantiatedSize, alignmentMask)
    = _getKeyPathClassAndInstanceSizeFromPattern(objectPtr, objectPtr)
  _sanityCheck(alignmentMask < MemoryLayout<Int>.alignment,
               "overalignment not implemented")

  let bufferPtr = objectPtr.advanced(by: keyPathObjectHeaderSize)
  let buffer = KeyPathBuffer(base: bufferPtr)
  let totalSize = buffer.data.count + MemoryLayout<Int>.size
  let bufferData = UnsafeMutableRawBufferPointer(
    start: bufferPtr,
    count: instantiatedSize)

  // Do the instantiation in place if the final object fits.
  if instantiatedSize <= totalSize {
    _instantiateKeyPathBuffer(buffer, bufferData, rootType, bufferPtr)

    _swift_instantiateInertHeapObject(objectPtr,
      unsafeBitCast(keyPathClass, to: OpaquePointer.self))
  } else {
    // Otherwise, we'll need to instantiate out-of-place.
    let object = _getKeyPath_instantiateOutOfLine(
      pattern: objectPtr,
      arguments: objectPtr,
      keyPathClass: keyPathClass,
      rootType: rootType,
      size: instantiatedSize,
      alignmentMask: alignmentMask)

    // Write a null pointer to the first word of the in-place buffer as
    // a signal that this isn't a valid object.
    // We rely on the heap object header size being >=2 words to get away with
    // this.
    assert(keyPathObjectHeaderSize >= MemoryLayout<Int>.size * 2)
    objectPtr.storeBytes(of: nil, as: UnsafeRawPointer?.self)

    // Put the pointer to the out-of-line object in the second word.
    objectPtr.storeBytes(of: object, toByteOffset: MemoryLayout<Int>.size,
                         as: UnsafeRawPointer.self)
  }
}

internal typealias MetadataAccessor =
  @convention(c) (UnsafeRawPointer) -> UnsafeRawPointer

internal func _getKeyPathClassAndInstanceSizeFromPattern(
  _ pattern: UnsafeRawPointer,
  _ arguments: UnsafeRawPointer
) -> (
  keyPathClass: AnyKeyPath.Type,
  rootType: Any.Type,
  size: Int,
  alignmentMask: Int
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
  var didChain = false

  let bufferPtr = pattern.advanced(by: keyPathObjectHeaderSize)
  var buffer = KeyPathBuffer(base: bufferPtr)
  var size = buffer.data.count + MemoryLayout<Int>.size

  while true {
    let header = buffer.pop(RawKeyPathComponent.Header.self)

    // Ensure that we pop an amount of data consistent with what
    // RawKeyPathComponent.Header.patternComponentBodySize computes.
    var bufferSizeBefore = 0
    var expectedPop = 0

    _sanityCheck({
      bufferSizeBefore = buffer.data.count
      expectedPop = header.patternComponentBodySize
      return true
    }())

    func setComputedCapability(for header: RawKeyPathComponent.Header) {
      let settable = header.isComputedSettable
      let mutating = header.isComputedMutating

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
    }

    switch header.kind {
    case .class:
      // The rest of the key path could be reference-writable.
      capability = .reference
      fallthrough
    case .struct:
      // No effect on the capability.
      // TODO: we should dynamically prevent "let" properties from being
      // reassigned.

      // Check the final instantiated size of the offset.
      if header.payload == RawKeyPathComponent.Header.unresolvedFieldOffsetPayload
        || header.payload == RawKeyPathComponent.Header.outOfLineOffsetPayload {
        _ = buffer.pop(UInt32.self)
      }
      if header.payload == RawKeyPathComponent.Header.unresolvedIndirectOffsetPayload {
        _ = buffer.pop(Int.self)
        // On 64-bit systems the pointer to the ivar offset variable is
        // pointer-sized and -aligned, but the resulting offset ought to be
        // 32 bits only and fit into padding between the 4-byte header and
        // pointer-aligned type word. We don't need this space after
        // instantiation.
        if MemoryLayout<Int>.size == 8 {
          size -= MemoryLayout<UnsafeRawPointer>.size
        }
      }

    case .external:
      // Look at the external property descriptor to see if we should take it
      // over the component given in the pattern.
      let genericParamCount = Int(header.payload)
      let descriptor = buffer.pop(UnsafeRawPointer.self)
      let descriptorHeader = descriptor.load(as: RawKeyPathComponent.Header.self)
      if descriptorHeader.isTrivialPropertyDescriptor {
        // If the descriptor is trivial, then use the local candidate.
        // Leave this external reference out of the final object size.
        size -= (2 + genericParamCount) * MemoryLayout<Int>.size
        // Skip the generic parameter accessors to get to the local candidate.
        _ = buffer.popRaw(size: MemoryLayout<Int>.size * genericParamCount,
                          alignment: Int.self)
        continue
      }

      // Drop this external reference...
      size -= (header.patternComponentBodySize
               + MemoryLayout<RawKeyPathComponent.Header>.size)
      _ = buffer.popRaw(size: MemoryLayout<Int>.size * genericParamCount,
                        alignment: Int.self)
      // ...and the local candidate, which is the component following this
      // one.
      let localCandidateHeader = buffer.pop(RawKeyPathComponent.Header.self)
      let localCandidateSize = localCandidateHeader.patternComponentBodySize
      size -= (localCandidateSize
               + MemoryLayout<RawKeyPathComponent.Header>.size)

      // (Note that we don't pop the local candidate from the pattern buffer
      // just yet, since we may need parts of it to instantiate the final
      // component in some cases below. It still ought to be consumed
      // in the computation below.)
      _sanityCheck({
        expectedPop += localCandidateSize
                    +  MemoryLayout<RawKeyPathComponent.Header>.size
        return true
      }())

      // Measure the instantiated size of the external component.
      let newComponentSize: Int
      switch descriptorHeader.kind {
      case .class:
        // A stored class property is reference-writable.
        // TODO: we should dynamically prevent "let" properties from being
        // reassigned (in both the struct and class cases).
        capability = .reference
        fallthrough
      case .struct:
        // Discard the local candidate.
        _ = buffer.popRaw(size: localCandidateSize,
                          alignment: Int32.self)

        // The final component will be a stored component with just an offset.
        // If the offset requires resolution, then it'll be stored out of
        // line after the header.
        if descriptorHeader.payload
            > RawKeyPathComponent.Header.maximumOffsetPayload {
          newComponentSize = MemoryLayout<RawKeyPathComponent.Header>.size
                           + MemoryLayout<UInt32>.size
        } else {
          newComponentSize = MemoryLayout<RawKeyPathComponent.Header>.size
        }

      case .computed:
        // The final component will be an instantiation of the computed
        // component.
        setComputedCapability(for: descriptorHeader)

        // If the external declaration is computed, and it takes captured
        // arguments, then we have to build a bit of a chimera. The canonical
        // identity and accessors come from the descriptor, but the argument
        // handling is still as described in the local candidate.
        if descriptorHeader.hasComputedArguments {
          // We always start with the buffer size and witnesses.
          var argumentBufferSize = MemoryLayout<Int>.size * 2

          if localCandidateHeader.kind == .computed
              && localCandidateHeader.hasComputedArguments {
            // We don't need the local candidate's accessors.
            _ /*id*/ = buffer.pop(UnsafeRawPointer.self)
            _ /*getter*/ = buffer.pop(UnsafeRawPointer.self)
            if localCandidateHeader.isComputedSettable {
              _ /*setter*/ = buffer.pop(UnsafeRawPointer.self)
            }

            // Get the instantiated size of the component's own argument
            // file.
            let getLayoutRaw = buffer.pop(UnsafeRawPointer.self)
            let _ /*witnesses*/ = buffer.pop(UnsafeRawPointer.self)
            let _ /*initializer*/ = buffer.pop(UnsafeRawPointer.self)

            let getLayout = unsafeBitCast(getLayoutRaw,
              to: RawKeyPathComponent.ComputedArgumentLayoutFn.self)

            let (addedSize, addedAlignmentMask) = getLayout(arguments)
            // TODO: Handle over-aligned values
            _sanityCheck(addedAlignmentMask < MemoryLayout<Int>.alignment,
                         "overaligned computed property element not supported")

            argumentBufferSize += addedSize

            // If the property descriptor also has generic arguments, we need
            // to store the size so we can invoke the local witnesses on
            // the arguments. We'll also store those generic arguments at
            // pointer alignment after the local candidate's arguments.
            if genericParamCount > 0 {
              argumentBufferSize =
                MemoryLayout<Int>._roundingUpToAlignment(argumentBufferSize)
              argumentBufferSize +=
                RawKeyPathComponent.Header.externalWithArgumentsExtraSize
            }
          } else {
            // If the local candidate has no arguments, then things are a
            // little easier. We only need to instantiate the generic arguments
            // for the external component's accessors.
            // Discard the local candidate.
            _ = buffer.popRaw(size: localCandidateSize,
                              alignment: UInt32.self)
          }

          // Add the property descriptor's generic arguments to the end, if
          // any.
          if genericParamCount > 0 {
            argumentBufferSize += MemoryLayout<Int>.size * genericParamCount
          }
          newComponentSize = MemoryLayout<RawKeyPathComponent.Header>.size
                           + descriptorHeader.propertyDescriptorBodySize
                           + argumentBufferSize
        } else {
          // If there aren't any captured arguments expected in the external
          // component, then we only need to adopt its accessors.
          // Discard the local candidate.
          _ = buffer.popRaw(size: localCandidateSize,
                            alignment: UInt32.self)
          // With no arguments, the instantiated size will be the
          // same as the pattern size.
          newComponentSize = MemoryLayout<RawKeyPathComponent.Header>.size
                           + descriptorHeader.propertyDescriptorBodySize
        }

      case .external, .optionalChain, .optionalForce, .optionalWrap:
        _sanityCheckFailure("should not appear as property descriptor")
      }

      // Round up to pointer alignment if there are following components.
      if !buffer.data.isEmpty {
        size += MemoryLayout<Int>._roundingUpToAlignment(newComponentSize)
      } else {
        size += newComponentSize
      }

    case .computed:
      let settable = header.isComputedSettable
      let hasArguments = header.hasComputedArguments

      setComputedCapability(for: header)

      _ = buffer.popRaw(size: MemoryLayout<Int>.size * (settable ? 3 : 2),
                        alignment: Int.self)

      // Get the instantiated size and alignment of the argument payload
      // by asking the layout function to compute it for our given argument
      // file.
      if hasArguments {
        let getLayoutRaw = buffer.pop(UnsafeRawPointer.self)
        let _ /*witnesses*/ = buffer.pop(UnsafeRawPointer.self)
        let _ /*initializer*/ = buffer.pop(UnsafeRawPointer.self)

        let getLayout = unsafeBitCast(getLayoutRaw,
          to: RawKeyPathComponent.ComputedArgumentLayoutFn.self)

        let (addedSize, addedAlignmentMask) = getLayout(arguments)
        // TODO: Handle over-aligned values
        _sanityCheck(addedAlignmentMask < MemoryLayout<Int>.alignment,
                     "overaligned computed property element not supported")

        // Argument payload replaces the space taken by the initializer
        // function pointer in the pattern.
        size += MemoryLayout<Int>._roundingUpToAlignment(addedSize)
              - MemoryLayout<Int>.size
      }

    case .optionalChain,
         .optionalWrap:
      // Chaining always renders the whole key path read-only.
      didChain = true
      break

    case .optionalForce:
      // No effect.
      break
    }

    // Check that we consumed the expected amount of data from the pattern.
    _sanityCheck(
      {
        // Round the amount of data we read up to alignment to include padding,
        // skewed by the header size.
        let popped = MemoryLayout<Int>._roundingUpToAlignment(
           bufferSizeBefore - buffer.data.count
           - RawKeyPathComponent.Header.pointerAlignmentSkew)
          + RawKeyPathComponent.Header.pointerAlignmentSkew

        return expectedPop == popped
      }(),
      """
      component size consumed during instance size measurement does not match \
      component size returned by patternComponentBodySize
      """)

    // Break if this is the last component.
    if buffer.data.count == 0 { break }

    // Pop the type accessor reference.
    _ = buffer.popRaw(size: MemoryLayout<Int>.size,
                      alignment: Int.self)
  }

  _sanityCheck(buffer.data.isEmpty, "didn't read entire pattern")

  // Chaining always renders the whole key path read-only.
  if didChain {
    capability = .readOnly
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

  return (keyPathClass: classTy, rootType: root,
          size: size,
          // FIXME: Handle overalignment
          alignmentMask: MemoryLayout<Int>._alignmentMask)
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
  var destData = UnsafeMutableRawBufferPointer(
    start: destHeaderPtr.advanced(by: MemoryLayout<Int>.size),
    count: origDestData.count - MemoryLayout<Int>.size)

  func pushDest<T>(_ value: T) {
    _sanityCheck(_isPOD(T.self))
    var value2 = value
    let size = MemoryLayout<T>.size
    let alignment = MemoryLayout<T>.alignment
    var baseAddress = destData.baseAddress.unsafelyUnwrapped
    var misalign = Int(bitPattern: baseAddress) % alignment
    if misalign != 0 {
      misalign = alignment - misalign
      baseAddress = baseAddress.advanced(by: misalign)
    }
    _memcpy(dest: baseAddress, src: &value2,
            size: UInt(size))
    destData = UnsafeMutableRawBufferPointer(
      start: baseAddress + size,
      count: destData.count - size - misalign)
  }

  // Track the triviality of the resulting object data.
  var isTrivial = true

  // Track where the reference prefix begins.
  var endOfReferencePrefixComponent: UnsafeMutableRawPointer? = nil
  var previousComponentAddr: UnsafeMutableRawPointer? = nil

  // Instantiate components that need it.
  var base: Any.Type = rootType
  // Some pattern forms are pessimistically larger than what we need in the
  // instantiated key path. Keep track of this.
  while true {
    let componentAddr = destData.baseAddress.unsafelyUnwrapped
    let header = patternBuffer.pop(RawKeyPathComponent.Header.self)

    // Ensure that we pop an amount of data consistent with what
    // RawKeyPathComponent.Header.patternComponentBodySize computes.
    var bufferSizeBefore = 0
    var expectedPop = 0

    _sanityCheck({
      bufferSizeBefore = patternBuffer.data.count
      expectedPop = header.patternComponentBodySize
      return true
    }())

    func tryToResolveOffset(header: RawKeyPathComponent.Header,
                            getOutOfLineOffset: () -> UInt32) {
      if header.payload == RawKeyPathComponent.Header.unresolvedFieldOffsetPayload {
        // Look up offset in type metadata. The value in the pattern is the
        // offset within the metadata object.
        let metadataPtr = unsafeBitCast(base, to: UnsafeRawPointer.self)
        let offsetOfOffset = getOutOfLineOffset()

        let offset: UInt32
        if (header.kind == .struct) {
          offset = UInt32(metadataPtr.load(fromByteOffset: Int(offsetOfOffset),
                                           as: UInt32.self))
        } else {
          offset = UInt32(metadataPtr.load(fromByteOffset: Int(offsetOfOffset),
                                           as: UInt.self))
        }

        // Rewrite the header for a resolved offset.
        var newHeader = header
        newHeader.payload = RawKeyPathComponent.Header.outOfLineOffsetPayload
        pushDest(newHeader)
        pushDest(offset)
        return
      }

      if header.payload == RawKeyPathComponent.Header.unresolvedIndirectOffsetPayload {
        // Look up offset in the indirectly-referenced variable we have a
        // pointer.
        let offsetVar = patternBuffer.pop(UnsafeRawPointer.self)
        let offsetValue = UInt32(offsetVar.load(as: UInt.self))
        // Rewrite the header for a resolved offset.
        var newHeader = header
        newHeader.payload = RawKeyPathComponent.Header.outOfLineOffsetPayload
        pushDest(newHeader)
        pushDest(offsetValue)
        return
      }

      // Otherwise, just transfer the pre-resolved component.
      pushDest(header)
      if header.payload == RawKeyPathComponent.Header.outOfLineOffsetPayload {
        let offset = getOutOfLineOffset() //patternBuffer.pop(UInt32.self)
        pushDest(offset)
      }
    }

    func tryToResolveComputedAccessors(header: RawKeyPathComponent.Header,
                                       accessorsBuffer: inout KeyPathBuffer) {
      // A nonmutating settable property can end the reference prefix and
      // makes the following key path potentially reference-writable.
      if header.isComputedSettable && !header.isComputedMutating {
        endOfReferencePrefixComponent = previousComponentAddr
      }

      // The ID may need resolution if the property is keyed by a selector.
      var newHeader = header
      var id = accessorsBuffer.pop(Int.self)
      switch header.payload
                         & RawKeyPathComponent.Header.computedIDResolutionMask {
      case RawKeyPathComponent.Header.computedIDResolved:
        // Nothing to do.
        break
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
      let getter = accessorsBuffer.pop(UnsafeRawPointer.self)
      pushDest(getter)
      if header.isComputedSettable {
        let setter = accessorsBuffer.pop(UnsafeRawPointer.self)
        pushDest(setter)
      }
    }

    func readComputedArgumentBuffer(argsBuffer: inout KeyPathBuffer)
        -> (getLayout: RawKeyPathComponent.ComputedArgumentLayoutFn,
            witnesses: UnsafePointer<ComputedArgumentWitnesses>,
            initializer: RawKeyPathComponent.ComputedArgumentInitializerFn) {
      let getLayoutRaw = argsBuffer.pop(UnsafeRawPointer.self)
      let getLayout = unsafeBitCast(getLayoutRaw,
        to: RawKeyPathComponent.ComputedArgumentLayoutFn.self)

      let witnesses = argsBuffer.pop(
        UnsafePointer<ComputedArgumentWitnesses>.self)

      if let _ = witnesses.pointee.destroy {
        isTrivial = false
      }

      let initializerRaw = argsBuffer.pop(UnsafeRawPointer.self)
      let initializer = unsafeBitCast(initializerRaw,
        to: RawKeyPathComponent.ComputedArgumentInitializerFn.self)

      return (getLayout, witnesses, initializer)
    }

    func tryToResolveComputedArguments(argsBuffer: inout KeyPathBuffer) {
      guard header.hasComputedArguments else { return }

      let (getLayout, witnesses, initializer)
        = readComputedArgumentBuffer(argsBuffer: &argsBuffer)

      // Carry over the arguments.
      let (size, alignmentMask) = getLayout(arguments)
      _sanityCheck(alignmentMask < MemoryLayout<Int>.alignment,
                   "overaligned computed arguments not implemented yet")

      // The real buffer stride will be rounded up to alignment.
      let stride = (size + alignmentMask) & ~alignmentMask
      pushDest(stride)
      pushDest(witnesses)

      _sanityCheck(Int(bitPattern: destData.baseAddress) & alignmentMask == 0,
                   "argument destination not aligned")
      initializer(arguments, destData.baseAddress.unsafelyUnwrapped)

      destData = UnsafeMutableRawBufferPointer(
        start: destData.baseAddress.unsafelyUnwrapped + stride,
        count: destData.count - stride)
    }

    switch header.kind {
    case .struct:
      // The offset may need to be resolved dynamically.
      tryToResolveOffset(header: header,
                         getOutOfLineOffset: { patternBuffer.pop(UInt32.self) })
    case .class:
      // Crossing a class can end the reference prefix, and makes the following
      // key path potentially reference-writable.
      endOfReferencePrefixComponent = previousComponentAddr
      // The offset may need to be resolved dynamically.
      tryToResolveOffset(header: header,
                         getOutOfLineOffset: { patternBuffer.pop(UInt32.self) })
    case .optionalChain,
         .optionalWrap,
         .optionalForce:
      // No instantiation necessary.
      pushDest(header)
      break
    case .computed:
      tryToResolveComputedAccessors(header: header,
                                    accessorsBuffer: &patternBuffer)
      tryToResolveComputedArguments(argsBuffer: &patternBuffer)
    case .external:
      // Look at the external property descriptor to see if we should take it
      // over the component given in the pattern.
      let genericParamCount = Int(header.payload)
      let descriptor = patternBuffer.pop(UnsafeRawPointer.self)
      var descriptorHeader = descriptor.load(as: RawKeyPathComponent.Header.self)

      // Save the generic arguments to the external descriptor.
      let descriptorGenericArgsBuf = patternBuffer.popRaw(
        size: MemoryLayout<Int>.size * genericParamCount,
        alignment: Int.self)

      if descriptorHeader.isTrivialPropertyDescriptor {
        // If the descriptor is trivial, then instantiate the local candidate.
        // Continue to keep reading from the buffer as if we started with the
        // local candidate.
        continue
      }

      // Grab the local candidate header. We may need parts of it to complete
      // the final component.
      let localCandidateHeader = patternBuffer.pop(RawKeyPathComponent.Header.self)
      let localCandidateSize = localCandidateHeader.patternComponentBodySize

      // ...though we still ought to fully consume it before proceeding.
      _sanityCheck({
        expectedPop += localCandidateSize
                    +  MemoryLayout<RawKeyPathComponent.Header>.size
        return true
      }())


      // Instantiate the component according to the external property
      // descriptor.
      switch descriptorHeader.kind {
      case .class:
        // Crossing a class can end the reference prefix, and makes the following
        // key path potentially reference-writable.
        endOfReferencePrefixComponent = previousComponentAddr
        fallthrough

      case .struct:
        // Drop the local candidate.
        _ = patternBuffer.popRaw(size: localCandidateSize,
                                 alignment: Int32.self)

        // Instantiate the offset using the info from the descriptor.
        tryToResolveOffset(header: descriptorHeader,
                           getOutOfLineOffset: {
                             descriptor.load(fromByteOffset: 4,
                                             as: UInt32.self)
                           })

      case .computed:
        var descriptorBuffer = KeyPathBuffer(
          partialData: UnsafeRawBufferPointer(
            start: descriptor + MemoryLayout<RawKeyPathComponent.Header>.size,
            count: descriptorHeader.propertyDescriptorBodySize))

        // If the external declaration is computed, and it takes captured
        // arguments, then we have to build a bit of a chimera. The canonical
        // identity and accessors come from the descriptor, but the argument
        // handling is still as described in the local candidate.
        if descriptorHeader.hasComputedArguments {
          // Loop through instantiating all the property descriptor's
          // generic arguments. We don't write
          // these immediately, because the computed header and accessors
          // come first, and if we're instantiating in-place,
          // they will overwrite the information in the pattern.
          let genericArgs: [UnsafeRawPointer]
            = (0 ..< genericParamCount).map {
              let instantiationFn = descriptorGenericArgsBuf
                .load(fromByteOffset: MemoryLayout<Int>.size * $0,
                      as: MetadataAccessor.self)
              return instantiationFn(arguments)
            }

          // If the descriptor has generic arguments, record this in the
          // header. We'll store the size of the external generic arguments
          // so we can invoke the local candidate's argument witnesses.
          let localCandidateHasArguments =
            localCandidateHeader.kind == .computed
              && localCandidateHeader.hasComputedArguments
          let descriptorHasArguments = genericParamCount > 0
          if localCandidateHasArguments && descriptorHasArguments {
            descriptorHeader.isComputedInstantiatedFromExternalWithArguments =
              true
          }

          // Bring in the accessors from the descriptor.
          tryToResolveComputedAccessors(header: descriptorHeader,
                                        accessorsBuffer: &descriptorBuffer)
          _sanityCheck(descriptorBuffer.data.isEmpty)

          if localCandidateHasArguments {
            // We don't need the local candidate's accessors.
            _ /*id*/ = patternBuffer.pop(UnsafeRawPointer.self)
            _ /*getter*/ = patternBuffer.pop(UnsafeRawPointer.self)
            if localCandidateHeader.isComputedSettable {
              _ /*setter*/ = patternBuffer.pop(UnsafeRawPointer.self)
            }

            // Instantiate the arguments from the local candidate.
            let (getLayout, witnesses, initializer) =
              readComputedArgumentBuffer(argsBuffer: &patternBuffer)

            // Carry over the arguments.
            let (baseSize, alignmentMask) = getLayout(arguments)
            _sanityCheck(alignmentMask < MemoryLayout<Int>.alignment,
                         "overaligned computed arguments not implemented yet")

            // The real buffer stride will be rounded up to alignment.
            var totalSize = (baseSize + alignmentMask) & ~alignmentMask

            // If the descriptor also has arguments, they'll be added to the
            // end with pointer alignment.
            if descriptorHasArguments {
              totalSize = MemoryLayout<Int>._roundingUpToAlignment(totalSize)
              totalSize += MemoryLayout<Int>.size * genericParamCount
            }

            pushDest(totalSize)
            pushDest(witnesses)

            // If the descriptor has arguments, store the size of its specific
            // arguments here, so we can drop them when trying to invoke
            // the component's witnesses.
            if descriptorHasArguments {
              pushDest(genericParamCount * MemoryLayout<Int>.size)
            }

            // Initialize the local candidate arguments here.
            _sanityCheck(Int(bitPattern: destData.baseAddress) & alignmentMask == 0,
                         "argument destination not aligned")
            initializer(arguments, destData.baseAddress.unsafelyUnwrapped)

            destData = UnsafeMutableRawBufferPointer(
              start: destData.baseAddress.unsafelyUnwrapped + baseSize,
              count: destData.count - baseSize)

          } else {
            // If the local candidate has no arguments, then things are a
            // little easier. We only need to instantiate the generic arguments
            // for the external component's accessors.
            // Discard the local candidate.
            _ = patternBuffer.popRaw(size: localCandidateSize,
                                     alignment: Int32.self)

            // Write out the header with the instantiated size and
            // witnesses of the descriptor.
            let stride = MemoryLayout<Int>.size * genericParamCount
            pushDest(stride)
            pushDest(__swift_keyPathGenericWitnessTable_addr())
          }
          // Write the descriptor's generic arguments.
          for arg in genericArgs {
            pushDest(arg)
          }
        } else {
          // Discard the local candidate.
          _ = patternBuffer.popRaw(size: localCandidateSize,
                                   alignment: Int32.self)

          // The final component is an instantiation of the computed
          // component from the descriptor.
          tryToResolveComputedAccessors(header: descriptorHeader,
                                        accessorsBuffer: &descriptorBuffer)
          _sanityCheck(descriptorBuffer.data.isEmpty)

          // We know there are no arguments to instantiate.
        }
      case .external, .optionalChain, .optionalForce, .optionalWrap:
        _sanityCheckFailure("should not appear as property descriptor")
      }
    }

    // Check that we consumed the expected amount of data from the pattern.
    _sanityCheck(
      {
        let popped = MemoryLayout<Int>._roundingUpToAlignment(
           bufferSizeBefore - patternBuffer.data.count
           - RawKeyPathComponent.Header.pointerAlignmentSkew)
          + RawKeyPathComponent.Header.pointerAlignmentSkew
        return expectedPop == popped
      }(),
      """
      component size consumed during instantiation does not match \
      component size returned by patternComponentBodySize
      """)

    // Break if this is the last component.
    if patternBuffer.data.count == 0 { break }

    // Resolve the component type.
    let componentTyAccessor = patternBuffer.pop(MetadataAccessor.self)
    base = unsafeBitCast(componentTyAccessor(arguments), to: Any.Type.self)
    pushDest(base)
    previousComponentAddr = componentAddr
  }

  // We should have traversed both buffers.
  _sanityCheck(patternBuffer.data.isEmpty, "did not read the entire pattern")
  _sanityCheck(destData.count == 0,
               "did not write to all of the allocated space")

  // Write out the header.
  let destHeader = KeyPathBuffer.Header(
    size: origDestData.count - MemoryLayout<Int>.size,
    trivial: isTrivial,
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
