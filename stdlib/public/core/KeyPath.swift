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

// NOTE: older runtimes had Swift.AnyKeyPath as the ObjC name.
// The two must coexist, so it was renamed. The old name must not be
// used in the new runtime. _TtCs11_AnyKeyPath is the mangled name for
// Swift._AnyKeyPath.
@_objcRuntimeName(_TtCs11_AnyKeyPath)

/// A type-erased key path, from any root type to any resulting value
/// type.
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
      if buffer.data.isEmpty { return }
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
        
        // Identity is equal to identity
        if aBuffer.data.isEmpty {
          return bBuffer.data.isEmpty
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
    _internalInvariantFailure("use _create(...)")
  }

  @usableFromInline
  internal class var _rootAndValueType: (root: Any.Type, value: Any.Type) {
    _abstract()
  }
  
  internal static func _create(
    capacityInBytes bytes: Int,
    initializedBy body: (UnsafeMutableRawBufferPointer) -> Void
  ) -> Self {
    _internalInvariant(bytes > 0 && bytes % 4 == 0,
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

      // The identity key path is effectively a stored keypath of type Self
      // at offset zero
      if buffer.data.isEmpty { return 0 }

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
  internal final func _projectReadOnly(from root: Root) -> Value {
    // TODO: For perf, we could use a local growable buffer instead of Any
    var curBase: Any = root
    return withBuffer {
      var buffer = $0
      if buffer.data.isEmpty {
        return unsafeBitCast(root, to: Value.self)
      }
      while true {
        let (rawComponent, optNextType) = buffer.next()
        let valueType = optNextType ?? Value.self
        let isLast = optNextType == nil
        
        func project<CurValue>(_ base: CurValue) -> Value? {
          func project2<NewValue>(_: NewValue.Type) -> Value? {
            switch rawComponent._projectReadOnly(base,
              to: NewValue.self, endingWith: Value.self) {
            case .continue(let newBase):
              if isLast {
                _internalInvariant(NewValue.self == Value.self,
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
  internal func _projectMutableAddress(from base: UnsafePointer<Root>)
      -> (pointer: UnsafeMutablePointer<Value>, owner: AnyObject?) {
    var p = UnsafeRawPointer(base)
    var type: Any.Type = Root.self
    var keepAlive: AnyObject?
    
    return withBuffer {
      var buffer = $0
      
      _internalInvariant(!buffer.hasReferencePrefix,
                   "WritableKeyPath should not have a reference prefix")
      
      if buffer.data.isEmpty {
        return (
          UnsafeMutablePointer<Value>(
            mutating: p.assumingMemoryBound(to: Value.self)),
          nil)
      }

      while true {
        let (rawComponent, optNextType) = buffer.next()
        let nextType = optNextType ?? Value.self
        
        func project<CurValue>(_: CurValue.Type) {
          func project2<NewValue>(_: NewValue.Type) {
            p = rawComponent._projectMutableAddress(p,
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
  
  internal final override func _projectMutableAddress(
    from base: UnsafePointer<Root>
  ) -> (pointer: UnsafeMutablePointer<Value>, owner: AnyObject?) {
    // Since we're a ReferenceWritableKeyPath, we know we don't mutate the base
    // in practice.
    return _projectMutableAddress(from: base.pointee)
  }
  
  @usableFromInline
  internal final func _projectMutableAddress(from origBase: Root)
      -> (pointer: UnsafeMutablePointer<Value>, owner: AnyObject?) {
    var keepAlive: AnyObject?
    var address: UnsafeMutablePointer<Value> = withBuffer {
      var buffer = $0
      // Project out the reference prefix.
      var base: Any = origBase
      while buffer.hasReferencePrefix {
        let (rawComponent, optNextType) = buffer.next()
        _internalInvariant(optNextType != nil,
                     "reference prefix should not go to end of buffer")
        let nextType = optNextType.unsafelyUnwrapped
        
        func project<NewValue>(_: NewValue.Type) -> Any {
          func project2<CurValue>(_ base: CurValue) -> Any {
            return rawComponent._projectReadOnly(
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
                p = rawComponent._projectMutableAddress(p,
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

extension RawKeyPathComponent {
  internal var bodySize: Int {
    let ptrSize = MemoryLayout<Int>.size
    switch header.kind {
    case .struct, .class:
      if header.storedOffsetPayload == Header.outOfLineOffsetPayload {
        return 4 // overflowed
      }
      return 0
    case .external:
      _internalInvariantFailure("should be instantiated away")
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
    _internalInvariant(header.kind == .struct || header.kind == .class,
                 "no offset for this kind")
    // An offset too large to fit inline is represented by a signal and stored
    // in the body.
    if header.storedOffsetPayload == Header.outOfLineOffsetPayload {
      // Offset overflowed into body
      _internalInvariant(body.count >= MemoryLayout<UInt32>.size,
                   "component not big enough")
      return Int(body.load(as: UInt32.self))
    }
    return Int(header.storedOffsetPayload)
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
        _internalInvariantFailure("impossible")
      }
    case .external:
      _internalInvariantFailure("should have been instantiated away")
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
      _internalInvariantFailure("should have been instantiated away")
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
      if header.storedOffsetPayload == Header.outOfLineOffsetPayload {
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
      _internalInvariantFailure("should have been instantiated away")
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
        _internalInvariantFailure("should not have stopped key path projection")
      }
    }
  }

  internal func _projectReadOnly<CurValue, NewValue, LeafValue>(
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
      _internalInvariant(CurValue.self is AnyObject.Type,
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
      _internalInvariant(CurValue.self == Optional<NewValue>.self,
                   "should be unwrapping optional value")
      _internalInvariant(_isOptional(LeafValue.self),
                   "leaf result should be optional")
      if let baseValue = unsafeBitCast(base, to: Optional<NewValue>.self) {
        return .continue(baseValue)
      } else {
        // TODO: A more efficient way of getting the `none` representation
        // of a dynamically-optional type...
        return .break((Optional<()>.none as Any) as! LeafValue)
      }

    case .optionalForce:
      _internalInvariant(CurValue.self == Optional<NewValue>.self,
                   "should be unwrapping optional value")
      return .continue(unsafeBitCast(base, to: Optional<NewValue>.self)!)

    case .optionalWrap:
      _internalInvariant(NewValue.self == Optional<CurValue>.self,
                   "should be wrapping optional value")
      return .continue(
        unsafeBitCast(base as Optional<CurValue>, to: NewValue.self))
    }
  }

  internal func _projectMutableAddress<CurValue, NewValue>(
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
      _internalInvariant(isRoot,
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
      _internalInvariant(isRoot,
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
      _internalInvariant(CurValue.self == Optional<NewValue>.self,
                   "should be unwrapping an optional value")
      // Optional's layout happens to always put the payload at the start
      // address of the Optional value itself, if a value is present at all.
      let baseOptionalPointer
        = base.assumingMemoryBound(to: Optional<NewValue>.self)
      // Assert that a value exists
      _ = baseOptionalPointer.pointee!
      return base
    
    case .optionalChain, .optionalWrap, .get:
      _internalInvariantFailure("not a mutable key path component")
    }
  }
}

extension KeyPathBuffer {
  internal init(base: UnsafeRawPointer) {
    let header = base.load(as: Header.self)
    data = UnsafeRawBufferPointer(
      start: base + MemoryLayout<Int>.size,
      count: header.size)
    trivial = header.trivial
    hasReferencePrefix = header.hasReferencePrefix
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
    let header = _pop(from: &data, as: RawKeyPathComponent.Header.self)
    // Track if this is the last component of the reference prefix.
    if header.endOfReferencePrefix {
      _internalInvariant(self.hasReferencePrefix,
                   "beginMutation marker in non-reference-writable key path?")
      self.hasReferencePrefix = false
    }
    
    var component = RawKeyPathComponent(header: header, body: data)
    // Shrinkwrap the component buffer size.
    let size = component.bodySize
    component.body = UnsafeRawBufferPointer(start: component.body.baseAddress,
                                            count: size)
    _ = _pop(from: &data, as: Int8.self, count: size)

    // fetch type, which is in the buffer unless it's the final component
    let nextType: Any.Type?
    if data.isEmpty {
      nextType = nil
    } else {
      nextType = _pop(from: &data, as: Any.Type.self)
    }
    return (component, nextType)
  }
}

// MARK: Library intrinsics for projecting key paths.

@_silgen_name("swift_getAtPartialKeyPath")
public // COMPILER_INTRINSIC
func _getAtPartialKeyPath<Root>(
  root: Root,
  keyPath: PartialKeyPath<Root>
) -> Any {
  func open<Value>(_: Value.Type) -> Any {
    return _getAtKeyPath(root: root,
      keyPath: unsafeDowncast(keyPath, to: KeyPath<Root, Value>.self))
  }
  return _openExistential(type(of: keyPath).valueType, do: open)
}

@_silgen_name("swift_getAtAnyKeyPath")
public // COMPILER_INTRINSIC
func _getAtAnyKeyPath<RootValue>(
  root: RootValue,
  keyPath: AnyKeyPath
) -> Any? {
  let (keyPathRoot, keyPathValue) = type(of: keyPath)._rootAndValueType
  func openRoot<KeyPathRoot>(_: KeyPathRoot.Type) -> Any? {
    guard let rootForKeyPath = root as? KeyPathRoot else {
      return nil
    }
    func openValue<Value>(_: Value.Type) -> Any {
      return _getAtKeyPath(root: rootForKeyPath,
        keyPath: unsafeDowncast(keyPath, to: KeyPath<KeyPathRoot, Value>.self))
    }
    return _openExistential(keyPathValue, do: openValue)
  }
  return _openExistential(keyPathRoot, do: openRoot)
}

@_silgen_name("swift_getAtKeyPath")
public // COMPILER_INTRINSIC
func _getAtKeyPath<Root, Value>(
  root: Root,
  keyPath: KeyPath<Root, Value>
) -> Value {
  return keyPath._projectReadOnly(from: root)
}

// The release that ends the access scope is guaranteed to happen
// immediately at the end_apply call because the continuation is a
// runtime call with a manual release (access scopes cannot be extended).
@_silgen_name("_swift_modifyAtWritableKeyPath_impl")
public // runtime entrypoint
func _modifyAtWritableKeyPath_impl<Root, Value>(
  root: inout Root,
  keyPath: WritableKeyPath<Root, Value>
) -> (UnsafeMutablePointer<Value>, AnyObject?) {
  return keyPath._projectMutableAddress(from: &root)
}

// The release that ends the access scope is guaranteed to happen
// immediately at the end_apply call because the continuation is a
// runtime call with a manual release (access scopes cannot be extended).
@_silgen_name("_swift_modifyAtReferenceWritableKeyPath_impl")
public // runtime entrypoint
func _modifyAtReferenceWritableKeyPath_impl<Root, Value>(
  root: Root,
  keyPath: ReferenceWritableKeyPath<Root, Value>
) -> (UnsafeMutablePointer<Value>, AnyObject?) {
  return keyPath._projectMutableAddress(from: root)
}

@_silgen_name("swift_setAtWritableKeyPath")
public // COMPILER_INTRINSIC
func _setAtWritableKeyPath<Root, Value>(
  root: inout Root,
  keyPath: WritableKeyPath<Root, Value>,
  value: __owned Value
) {
  // TODO: we should be able to do this more efficiently than projecting.
  let (addr, owner) = keyPath._projectMutableAddress(from: &root)
  addr.pointee = value
  _fixLifetime(owner)
  // FIXME: this needs a deallocation barrier to ensure that the
  // release isn't extended, along with the access scope.
}

@_silgen_name("swift_setAtReferenceWritableKeyPath")
public // COMPILER_INTRINSIC
func _setAtReferenceWritableKeyPath<Root, Value>(
  root: Root,
  keyPath: ReferenceWritableKeyPath<Root, Value>,
  value: __owned Value
) {
  // TODO: we should be able to do this more efficiently than projecting.
  let (addr, owner) = keyPath._projectMutableAddress(from: root)
  addr.pointee = value
  _fixLifetime(owner)
  // FIXME: this needs a deallocation barrier to ensure that the
  // release isn't extended, along with the access scope.
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

      // If either operand is the identity key path, then we should return
      // the other operand back untouched.
      if leafBuffer.data.isEmpty {
        return unsafeDowncast(root, to: Result.self)
      }
      if rootBuffer.data.isEmpty {
        return unsafeDowncast(leaf, to: Result.self)
      }

      // Reserve room for the appended KVC string, if both key paths are
      // KVC-compatible.
      let appendedKVCLength: Int, rootKVCLength: Int, leafKVCLength: Int

      if let rootPtr = root._kvcKeyPathStringPtr,
         let leafPtr = leaf._kvcKeyPathStringPtr {
        rootKVCLength = Int(_swift_stdlib_strlen(rootPtr))
        leafKVCLength = Int(_swift_stdlib_strlen(leafPtr))
        // root + "." + leaf
        appendedKVCLength = rootKVCLength + 1 + leafKVCLength + 1
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
        
        _internalInvariant(destBuffer.isEmpty,
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
