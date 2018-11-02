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

@usableFromInline @_transparent
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
@_fixed_layout // FIXME(sil-serialize-all)
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

  @usableFromInline // FIXME(sil-serialize-all)
  internal final var _kvcKeyPathStringPtr: UnsafePointer<CChar>?
  
  /// The hash value.
  @inlinable
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
  
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
  public var _kvcKeyPathString: String? {
    guard let ptr = _kvcKeyPathStringPtr else { return nil }

    return String(validatingUTF8: ptr)
  }
  
  // MARK: Implementation details
  
  // Prevent normal initialization. We use tail allocation via
  // allocWithTailElems().
  @inlinable // FIXME(sil-serialize-all)
  internal init() {
    _sanityCheckFailure("use _create(...)")
  }

  @inlinable // FIXME(sil-serialize-all)
  deinit {}
  
  // internal-with-availability
  @inlinable // FIXME(sil-serialize-all)
  public class var _rootAndValueType: (root: Any.Type, value: Any.Type) {
    _abstract()
  }
  
  @inlinable // FIXME(sil-serialize-all)
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
  
  @inlinable // FIXME(sil-serialize-all)
  internal func withBuffer<T>(_ f: (KeyPathBuffer) throws -> T) rethrows -> T {
    defer { _fixLifetime(self) }
    
    let base = UnsafeRawPointer(Builtin.projectTailElems(self, Int32.self))
    return try f(KeyPathBuffer(base: base))
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _storedInlineOffset: Int? {
    return withBuffer {
      var buffer = $0
      var offset = 0
      while true {
        let (rawComponent, optNextType) = buffer.next()
        switch rawComponent.header.kind {
        case .struct:
          offset += rawComponent._structOrClassOffset

        case .class, .computed, .optionalChain, .optionalForce, .optionalWrap:
          return .none
        }

        if optNextType == nil { return .some(offset) }
      }
    }
  }
}

/// A partially type-erased key path, from a concrete root type to any
/// resulting value type.
@_fixed_layout // FIXME(sil-serialize-all)
public class PartialKeyPath<Root>: AnyKeyPath { }

// MARK: Concrete implementations
@_frozen // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal enum KeyPathKind { case readOnly, value, reference }

/// A key path from a specific root type to a specific resulting value type.
@_fixed_layout // FIXME(sil-serialize-all)
public class KeyPath<Root, Value>: PartialKeyPath<Root> {
  public typealias _Root = Root
  public typealias _Value = Value

  @inlinable // FIXME(sil-serialize-all)
  public final override class var _rootAndValueType: (
    root: Any.Type,
    value: Any.Type
  ) {
    return (Root.self, Value.self)
  }
  
  // MARK: Implementation
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias Kind = KeyPathKind
  @inlinable // FIXME(sil-serialize-all)
  internal class var kind: Kind { return .readOnly }
  
  @inlinable // FIXME(sil-serialize-all)
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
  
  @inlinable // FIXME(sil-serialize-all)
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
  
  @inlinable // FIXME(sil-serialize-all)
  deinit {
    withBuffer { $0.destroy() }
  }
}

/// A key path that supports reading from and writing to the resulting value.
@_fixed_layout // FIXME(sil-serialize-all)
public class WritableKeyPath<Root, Value>: KeyPath<Root, Value> {
  // MARK: Implementation detail
  
  @inlinable // FIXME(sil-serialize-all)
  internal override class var kind: Kind { return .value }

  // `base` is assumed to be undergoing a formal access for the duration of the
  // call, so must not be mutated by an alias
  @inlinable // FIXME(sil-serialize-all)
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
@_fixed_layout // FIXME(sil-serialize-all)
public class ReferenceWritableKeyPath<
  Root, Value
> : WritableKeyPath<Root, Value> {
  // MARK: Implementation detail

  @inlinable // FIXME(sil-serialize-all)
  internal final override class var kind: Kind { return .reference }
  
  @inlinable // FIXME(sil-serialize-all)
  internal final override func projectMutableAddress(
    from base: UnsafePointer<Root>
  ) -> (pointer: UnsafeMutablePointer<Value>, owner: AnyObject?) {
    // Since we're a ReferenceWritableKeyPath, we know we don't mutate the base
    // in practice.
    return projectMutableAddress(from: base.pointee)
  }
  
  @inlinable // FIXME(sil-serialize-all)
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

@_frozen // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
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

@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal struct ComputedPropertyID: Hashable {
  @inlinable // FIXME(sil-serialize-all)
  internal init(value: Int, isStoredProperty: Bool, isTableOffset: Bool) {
    self.value = value
    self.isStoredProperty = isStoredProperty
    self.isTableOffset = isTableOffset
  }

  @usableFromInline // FIXME(sil-serialize-all)
  internal var value: Int
  @usableFromInline // FIXME(sil-serialize-all)
  internal var isStoredProperty: Bool
  @usableFromInline // FIXME(sil-serialize-all)
  internal var isTableOffset: Bool

  @inlinable // FIXME(sil-serialize-all)
  internal static func ==(
    x: ComputedPropertyID, y: ComputedPropertyID
  ) -> Bool {
    return x.value == y.value
      && x.isStoredProperty == y.isStoredProperty
      && x.isTableOffset == x.isTableOffset
  }

  @inlinable
  internal func hash(into hasher: inout Hasher) {
    hasher.combine(value)
    hasher.combine(isStoredProperty)
    hasher.combine(isTableOffset)
  }
}

@usableFromInline // FIXME(sil-serialize-all)
@_fixed_layout // FIXME(sil-serialize-all)
internal struct ComputedArgumentWitnesses {
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias Destroy = @convention(thin)
    (_ instanceArguments: UnsafeMutableRawPointer, _ size: Int) -> ()
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias Copy = @convention(thin)
    (_ srcInstanceArguments: UnsafeRawPointer,
     _ destInstanceArguments: UnsafeMutableRawPointer,
     _ size: Int) -> ()
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias Equals = @convention(thin)
    (_ xInstanceArguments: UnsafeRawPointer,
     _ yInstanceArguments: UnsafeRawPointer,
     _ size: Int) -> Bool
  // FIXME(hasher) Combine to an inout Hasher instead
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias Hash = @convention(thin)
    (_ instanceArguments: UnsafeRawPointer,
     _ size: Int) -> Int

  @usableFromInline // FIXME(sil-serialize-all)
  internal let destroy: Destroy?
  @usableFromInline // FIXME(sil-serialize-all)
  internal let copy: Copy
  @usableFromInline // FIXME(sil-serialize-all)
  internal let equals: Equals
  @usableFromInline // FIXME(sil-serialize-all)
  internal let hash: Hash
}

@_frozen // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal enum KeyPathComponent: Hashable {
  @_fixed_layout // FIXME(sil-serialize-all)
  @usableFromInline // FIXME(sil-serialize-all)
  internal struct ArgumentRef {
    @inlinable // FIXME(sil-serialize-all)
    internal init(
      data: UnsafeRawBufferPointer,
      witnesses: UnsafePointer<ComputedArgumentWitnesses>
    ) {
      self.data = data
      self.witnesses = witnesses
    }

    @usableFromInline // FIXME(sil-serialize-all)
    internal var data: UnsafeRawBufferPointer
    @usableFromInline // FIXME(sil-serialize-all)
    internal var witnesses: UnsafePointer<ComputedArgumentWitnesses>
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

  @inlinable // FIXME(sil-serialize-all)
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
          arg1.data.count)
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
          argument.data.count)
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
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal final class ClassHolder<ProjectionType> {

  /// The type of the scratch record passed to the runtime to record
  /// accesses to guarantee exlcusive access.
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias AccessRecord = Builtin.UnsafeValueBuffer

  @usableFromInline // FIXME(sil-serialize-all)
  internal var previous: AnyObject?
  @usableFromInline // FIXME(sil-serialize-all)
  internal var instance: AnyObject

  @inlinable // FIXME(sil-serialize-all)
  internal init(previous: AnyObject?, instance: AnyObject) {
    self.previous = previous
    self.instance = instance
  }

  @inlinable // FIXME(sil-serialize-all)
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

  @inlinable // FIXME(sil-serialize-all)
  deinit {
    let accessRecordPtr = Builtin.projectTailElems(self, AccessRecord.self)

    // Ends the access begun in _create().
    Builtin.endUnpairedAccess(accessRecordPtr)
  }
}

// A class that triggers writeback to a pointer when destroyed.
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal final class MutatingWritebackBuffer<CurValue, NewValue> {
  @usableFromInline // FIXME(sil-serialize-all)
  internal let previous: AnyObject?
  @usableFromInline // FIXME(sil-serialize-all)
  internal let base: UnsafeMutablePointer<CurValue>
  @usableFromInline // FIXME(sil-serialize-all)
  internal let set: @convention(thin) (NewValue, inout CurValue, UnsafeRawPointer, Int) -> ()
  @usableFromInline // FIXME(sil-serialize-all)
  internal let argument: UnsafeRawPointer
  @usableFromInline // FIXME(sil-serialize-all)
  internal let argumentSize: Int
  @usableFromInline // FIXME(sil-serialize-all)
  internal var value: NewValue

  @inlinable // FIXME(sil-serialize-all)
  deinit {
    set(value, &base.pointee, argument, argumentSize)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal
  init(previous: AnyObject?,
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
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal final class NonmutatingWritebackBuffer<CurValue, NewValue> {
  @usableFromInline // FIXME(sil-serialize-all)
  internal let previous: AnyObject?
  @usableFromInline // FIXME(sil-serialize-all)
  internal let base: CurValue
  @usableFromInline // FIXME(sil-serialize-all)
  internal let set: @convention(thin) (NewValue, CurValue, UnsafeRawPointer, Int) -> ()
  @usableFromInline // FIXME(sil-serialize-all)
  internal let argument: UnsafeRawPointer
  @usableFromInline // FIXME(sil-serialize-all)
  internal let argumentSize: Int
  @usableFromInline // FIXME(sil-serialize-all)
  internal var value: NewValue

  @inlinable // FIXME(sil-serialize-all)
  deinit {
    set(value, base, argument, argumentSize)
  }

  @inlinable // FIXME(sil-serialize-all)
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

@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal struct RawKeyPathComponent {
  @inlinable // FIXME(sil-serialize-all)
  internal init(header: Header, body: UnsafeRawBufferPointer) {
    self.header = header
    self.body = body
  }

  @usableFromInline // FIXME(sil-serialize-all)
  internal var header: Header
  @usableFromInline // FIXME(sil-serialize-all)
  internal var body: UnsafeRawBufferPointer
  
  @_fixed_layout // FIXME(sil-serialize-all)
  @usableFromInline // FIXME(sil-serialize-all)
  internal struct Header {
    @inlinable // FIXME(sil-serialize-all)
    internal static var payloadMask: UInt32 {
      return _SwiftKeyPathComponentHeader_PayloadMask
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var discriminatorMask: UInt32 {
      return _SwiftKeyPathComponentHeader_DiscriminatorMask
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var discriminatorShift: UInt32 {
      return _SwiftKeyPathComponentHeader_DiscriminatorShift
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var structTag: UInt32 {
      return _SwiftKeyPathComponentHeader_StructTag
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var computedTag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedTag
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var classTag: UInt32 {
      return _SwiftKeyPathComponentHeader_ClassTag
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var optionalTag: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalTag
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var optionalChainPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalChainPayload
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var optionalWrapPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalWrapPayload
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var optionalForcePayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OptionalForcePayload
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var endOfReferencePrefixFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_EndOfReferencePrefixFlag
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var outOfLineOffsetPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_OutOfLineOffsetPayload
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var unresolvedFieldOffsetPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_UnresolvedFieldOffsetPayload
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var unresolvedIndirectOffsetPayload: UInt32 {
      return _SwiftKeyPathComponentHeader_UnresolvedIndirectOffsetPayload
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var computedMutatingFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedMutatingFlag
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var computedSettableFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedSettableFlag
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var computedIDByStoredPropertyFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDByStoredPropertyFlag
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var computedIDByVTableOffsetFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDByVTableOffsetFlag
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var computedHasArgumentsFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedHasArgumentsFlag
    }

    @inlinable // FIXME(sil-serialize-all)
    internal static var computedIDResolutionMask: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDResolutionMask
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var computedIDResolved: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDResolved
    }
    @inlinable // FIXME(sil-serialize-all)
    internal static var computedIDUnresolvedIndirectPointer: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDUnresolvedIndirectPointer
    }
    
    @usableFromInline // FIXME(sil-serialize-all)
    internal var _value: UInt32
    
    @inlinable // FIXME(sil-serialize-all)
    internal var discriminator: UInt32 {
      return (_value & Header.discriminatorMask) >> Header.discriminatorShift
    }
    @inlinable // FIXME(sil-serialize-all)
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
    @inlinable // FIXME(sil-serialize-all)
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

    @inlinable // FIXME(sil-serialize-all)
    internal var kind: KeyPathComponentKind {
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

    // The component header is 4 bytes, but may be followed by an aligned
    // pointer field for some kinds of component, forcing padding.
    @inlinable // FIXME(sil-serialize-all)
    internal static var pointerAlignmentSkew: Int {
      return MemoryLayout<Int>.size - MemoryLayout<Int32>.size
    }

  }

  @inlinable // FIXME(sil-serialize-all)
  internal var bodySize: Int {
    switch header.kind {
    case .struct, .class:
      if header.payload == Header.payloadMask { return 4 } // overflowed
      return 0
    case .optionalChain, .optionalForce, .optionalWrap:
      return 0
    case .computed:
      let ptrSize = MemoryLayout<Int>.size
      // align to pointer, minimum two pointers for id and get
      var total = Header.pointerAlignmentSkew + ptrSize * 2
      // additional word for a setter
      if header.payload & Header.computedSettableFlag != 0 {
        total += ptrSize
      }
      // include the argument size
      if header.payload & Header.computedHasArgumentsFlag != 0 {
        // two words for argument header: size, witnesses
        total += ptrSize * 2
        // size of argument area
        total += _computedArgumentSize
      }
      return total
    }
  }

  @inlinable // FIXME(sil-serialize-all)
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

  @inlinable // FIXME(sil-serialize-all)
  internal var _computedIDValue: Int {
    _sanityCheck(header.kind == .computed,
                 "not a computed property")
    return body.load(fromByteOffset: Header.pointerAlignmentSkew,
                     as: Int.self)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _computedID: ComputedPropertyID {
    let payload = header.payload
    return ComputedPropertyID(
      value: _computedIDValue,
      isStoredProperty: payload & Header.computedIDByStoredPropertyFlag != 0,
      isTableOffset: payload & Header.computedIDByVTableOffsetFlag != 0)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _computedGetter: UnsafeRawPointer {
    _sanityCheck(header.kind == .computed,
                 "not a computed property")

    return body.load(
      fromByteOffset: Header.pointerAlignmentSkew + MemoryLayout<Int>.size,
      as: UnsafeRawPointer.self)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _computedSetter: UnsafeRawPointer {
    _sanityCheck(header.kind == .computed,
                 "not a computed property")
    _sanityCheck(header.payload & Header.computedSettableFlag != 0,
                 "not a settable property")

    return body.load(
      fromByteOffset: Header.pointerAlignmentSkew + MemoryLayout<Int>.size * 2,
      as: UnsafeRawPointer.self)
  }

  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias ComputedArgumentLayoutFn = @convention(thin)
    (_ patternArguments: UnsafeRawPointer) -> (size: Int, alignmentMask: Int)
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias ComputedArgumentInitializerFn = @convention(thin)
    (_ patternArguments: UnsafeRawPointer,
     _ instanceArguments: UnsafeMutableRawPointer) -> ()

  @inlinable // FIXME(sil-serialize-all)
  internal var _computedArgumentHeaderPointer: UnsafeRawPointer {
    _sanityCheck(header.kind == .computed,
                 "not a computed property")
    _sanityCheck(header.payload & Header.computedHasArgumentsFlag != 0,
                 "no arguments")

    return body.baseAddress.unsafelyUnwrapped
      + Header.pointerAlignmentSkew
      + MemoryLayout<Int>.size *
         (header.payload & Header.computedSettableFlag != 0 ? 3 : 2)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _computedArgumentSize: Int {
    return _computedArgumentHeaderPointer.load(as: Int.self)
  }
  @inlinable // FIXME(sil-serialize-all)
  internal
  var _computedArgumentWitnesses: UnsafePointer<ComputedArgumentWitnesses> {
    return _computedArgumentHeaderPointer.load(
      fromByteOffset: MemoryLayout<Int>.size,
      as: UnsafePointer<ComputedArgumentWitnesses>.self)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _computedArguments: UnsafeRawPointer {
    return _computedArgumentHeaderPointer + MemoryLayout<Int>.size * 2
  }
  @inlinable // FIXME(sil-serialize-all)
  internal var _computedMutableArguments: UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(mutating: _computedArguments)
  }

  @inlinable // FIXME(sil-serialize-all)
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
      let isSettable = header.payload & Header.computedSettableFlag != 0
      let isMutating = header.payload & Header.computedMutatingFlag != 0

      let id = _computedID
      let get = _computedGetter
      // Argument value is unused if there are no arguments, so pick something
      // likely to already be in a register as a default.
      let argument: KeyPathComponent.ArgumentRef?
      if header.payload & Header.computedHasArgumentsFlag != 0 {
        argument = KeyPathComponent.ArgumentRef(
          data: UnsafeRawBufferPointer(start: _computedArguments,
                                       count: _computedArgumentSize),
          witnesses: _computedArgumentWitnesses)
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
    }
  }

  @inlinable // FIXME(sil-serialize-all)
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
      if header.payload & Header.computedHasArgumentsFlag != 0,
         let destructor = _computedArgumentWitnesses.pointee.destroy {
        destructor(_computedMutableArguments, _computedArgumentSize)
      }
    }
  }

  @inlinable // FIXME(sil-serialize-all)
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
                        toByteOffset: MemoryLayout<Int>.size,
                        as: Int.self)
      buffer.storeBytes(of: _computedGetter,
                        toByteOffset: 2 * MemoryLayout<Int>.size,
                        as: UnsafeRawPointer.self)

      var addedSize = MemoryLayout<Int>.size * 2

      if header.payload & Header.computedSettableFlag != 0 {
        buffer.storeBytes(of: _computedSetter,
                          toByteOffset: MemoryLayout<Int>.size * 3,
                          as: UnsafeRawPointer.self)
        addedSize += MemoryLayout<Int>.size
      }

      if header.payload & Header.computedHasArgumentsFlag != 0 {
        let argumentSize = _computedArgumentSize
        buffer.storeBytes(of: argumentSize,
                          toByteOffset: addedSize + MemoryLayout<Int>.size,
                          as: Int.self)
        buffer.storeBytes(of: _computedArgumentWitnesses,
                          toByteOffset: addedSize + MemoryLayout<Int>.size * 2,
                          as: UnsafePointer<ComputedArgumentWitnesses>.self)
        _computedArgumentWitnesses.pointee.copy(
          _computedArguments,
          buffer.baseAddress.unsafelyUnwrapped + addedSize
                                               + MemoryLayout<Int>.size * 3,
          argumentSize)
        addedSize += MemoryLayout<Int>.size * 2 + argumentSize
      }

      componentSize += addedSize
    }
    buffer = UnsafeMutableRawBufferPointer(
      start: buffer.baseAddress.unsafelyUnwrapped + componentSize,
      count: buffer.count - componentSize)
  }

  @_frozen // FIXME(sil-serialize-all)
  @usableFromInline
  internal enum ProjectionResult<NewValue, LeafValue> {
    /// Continue projecting the key path with the given new value.
    case `continue`(NewValue)
    /// Stop projecting the key path and use the given value as the final
    /// result of the projection.
    case `break`(LeafValue)

    @inlinable // FIXME(sil-serialize-all)
    internal var assumingContinue: NewValue {
      switch self {
      case .continue(let x):
        return x
      case .break:
        _sanityCheckFailure("should not have stopped key path projection")
      }
    }
  }

  @inlinable // FIXME(sil-serialize-all)
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

  @inlinable // FIXME(sil-serialize-all)
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

@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal struct KeyPathBuffer {
  @usableFromInline // FIXME(sil-serialize-all)
  internal var data: UnsafeRawBufferPointer
  @usableFromInline // FIXME(sil-serialize-all)
  internal var trivial: Bool
  @usableFromInline // FIXME(sil-serialize-all)
  internal var hasReferencePrefix: Bool

  @inlinable // FIXME(sil-serialize-all)
  internal var mutableData: UnsafeMutableRawBufferPointer {
    return UnsafeMutableRawBufferPointer(mutating: data)
  }

  @_fixed_layout // FIXME(sil-serialize-all)
  @usableFromInline // FIXME(sil-serialize-all)
  internal struct Header {
    @usableFromInline // FIXME(sil-serialize-all)
    internal var _value: UInt32
    
    @usableFromInline // FIXME(sil-serialize-all)
    internal static var sizeMask: UInt32 {
      return _SwiftKeyPathBufferHeader_SizeMask
    }
    @usableFromInline // FIXME(sil-serialize-all)
    internal static var reservedMask: UInt32 {
      return _SwiftKeyPathBufferHeader_ReservedMask
    }
    @usableFromInline // FIXME(sil-serialize-all)
    internal static var trivialFlag: UInt32 {
      return _SwiftKeyPathBufferHeader_TrivialFlag
    }
    @usableFromInline // FIXME(sil-serialize-all)
    internal static var hasReferencePrefixFlag: UInt32 {
      return _SwiftKeyPathBufferHeader_HasReferencePrefixFlag
    }

    @inlinable // FIXME(sil-serialize-all)
    internal init(size: Int, trivial: Bool, hasReferencePrefix: Bool) {
      _sanityCheck(size <= Int(Header.sizeMask), "key path too big")
      _value = UInt32(size)
        | (trivial ? Header.trivialFlag : 0)
        | (hasReferencePrefix ? Header.hasReferencePrefixFlag : 0)
    }

    @inlinable // FIXME(sil-serialize-all)
    internal var size: Int { return Int(_value & Header.sizeMask) }
    @inlinable // FIXME(sil-serialize-all)
    internal var trivial: Bool { return _value & Header.trivialFlag != 0 }
    @inlinable // FIXME(sil-serialize-all)
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
    @inlinable // FIXME(sil-serialize-all)
    internal var instantiableInLine: Bool {
      return trivial
    }

    @inlinable // FIXME(sil-serialize-all)
    internal func validateReservedBits() {
      _precondition(_value & Header.reservedMask == 0,
                    "Reserved bits set to an unexpected bit pattern")
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init(base: UnsafeRawPointer) {
    let header = base.load(as: Header.self)
    data = UnsafeRawBufferPointer(
      start: base + MemoryLayout<Int>.size,
      count: header.size)
    trivial = header.trivial
    hasReferencePrefix = header.hasReferencePrefix
  }
  
  @inlinable // FIXME(sil-serialize-all)
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
  
  @inlinable // FIXME(sil-serialize-all)
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
    _ = popRaw(size: size, alignment: 1)

    // fetch type, which is in the buffer unless it's the final component
    let nextType: Any.Type?
    if data.count == 0 {
      nextType = nil
    } else {
      nextType = pop(Any.Type.self)
    }
    return (component, nextType)
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func pop<T>(_ type: T.Type) -> T {
    _sanityCheck(_isPOD(T.self), "should be POD")
    let raw = popRaw(size: MemoryLayout<T>.size,
                     alignment: MemoryLayout<T>.alignment)
    let resultBuf = UnsafeMutablePointer<T>.allocate(capacity: 1)
    _memcpy(dest: resultBuf,
            src: raw.baseAddress.unsafelyUnwrapped,
            size: UInt(MemoryLayout<T>.size))
    let result = resultBuf.pointee
    resultBuf.deallocate()
    return result
  }
  @inlinable // FIXME(sil-serialize-all)
  internal
  mutating func popRaw(size: Int, alignment: Int) -> UnsafeRawBufferPointer {
    var baseAddress = data.baseAddress.unsafelyUnwrapped
    var misalignment = Int(bitPattern: baseAddress) % alignment
    if misalignment != 0 {
      misalignment = alignment - misalignment
      baseAddress += misalignment
    }

    let result = UnsafeRawBufferPointer(start: baseAddress, count: size)
    data = UnsafeRawBufferPointer(
      start: baseAddress + size,
      count: data.count - size - misalignment
    )
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

@inlinable // FIXME(sil-serialize-all)
public // COMPILER_INTRINSIC
func _projectKeyPathReadOnly<Root, Value>(
  root: Root,
  keyPath: KeyPath<Root, Value>
) -> Value {
  return keyPath.projectReadOnly(from: root)
}

@inlinable // FIXME(sil-serialize-all)
public // COMPILER_INTRINSIC
func _projectKeyPathWritable<Root, Value>(
  root: UnsafeMutablePointer<Root>,
  keyPath: WritableKeyPath<Root, Value>
) -> (UnsafeMutablePointer<Value>, AnyObject?) {
  return keyPath.projectMutableAddress(from: root)
}

@inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
  public func appending<Root, Value, AppendedValue>(
    path: WritableKeyPath<Value, AppendedValue>
  ) -> ReferenceWritableKeyPath<Root, AppendedValue>
  where Self == ReferenceWritableKeyPath<Root, Value> {
    return _appendingKeyPaths(root: self, leaf: path)
  }
}

// internal-with-availability
@inlinable // FIXME(sil-serialize-all)
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
@inlinable // FIXME(sil-serialize-all)
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
      let alignMask = MemoryLayout<Int>.alignment - 1
      let rootSize = (rootBuffer.data.count + alignMask) & ~alignMask
      let resultSize = rootSize + leafBuffer.data.count
        + 2 * MemoryLayout<Int>.size
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

@inlinable // FIXME(sil-serialize-all)
internal var keyPathObjectHeaderSize: Int {
  return MemoryLayout<HeapObject>.size + MemoryLayout<Int>.size
}

// Runtime entry point to instantiate a key path object.
@inlinable // FIXME(sil-serialize-all)
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
    // TODO: This will be unnecessary once we support global objects with inert
    // refcounting.
    let object = Unmanaged<AnyKeyPath>.fromOpaque(patternPtr)
    _ = object.retain()
    return UnsafeRawPointer(patternPtr)
  }

  // Otherwise, instantiate a new key path object modeled on the pattern.
  return _getKeyPath_instantiatedOutOfLine(patternPtr, arguments)
}

@inlinable // FIXME(sil-serialize-all)
internal func _getKeyPath_instantiatedOutOfLine(
  _ pattern: UnsafeRawPointer,
  _ arguments: UnsafeRawPointer)
    -> UnsafeRawPointer {
  // Do a pass to determine the class of the key path we'll be instantiating
  // and how much space we'll need for it.
  let (keyPathClass, rootType, size, alignmentMask)
    = _getKeyPathClassAndInstanceSizeFromPattern(pattern, arguments)
  _sanityCheck(alignmentMask < MemoryLayout<Int>.alignment,
               "overalignment not implemented")

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

@inlinable // FIXME(sil-serialize-all)
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

  // TODO: Eventually, we'll need to handle cases where the instantiated
  // key path has a larger size than the pattern (because it involves
  // resilient types, for example), and fall back to out-of-place instantiation
  // when that happens.

  _sanityCheck(instantiatedSize <= totalSize,
               "size-increasing in-place instantiation not implemented")

  // Instantiate the pattern in place.
  _instantiateKeyPathBuffer(buffer, bufferData, rootType, bufferPtr)

  _swift_instantiateInertHeapObject(objectPtr,
    unsafeBitCast(keyPathClass, to: OpaquePointer.self))
}

@usableFromInline // FIXME(sil-serialize-all)
internal typealias MetadataAccessor =
  @convention(c) (UnsafeRawPointer) -> UnsafeRawPointer

@inlinable // FIXME(sil-serialize-all)
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
  var alignmentMask = MemoryLayout<Int>.alignment - 1

  while true {
    let header = buffer.pop(RawKeyPathComponent.Header.self)

    func popOffset() {
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

      let hasArguments =
        header.payload & RawKeyPathComponent.Header.computedHasArgumentsFlag != 0

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

      _ = buffer.popRaw(size: MemoryLayout<Int>.size * (settable ? 3 : 2),
                        alignment: MemoryLayout<Int>.alignment)

      // Get the instantiated size and alignment of the argument payload
      // by asking the layout function to compute it for our given argument
      // file.
      if hasArguments {
        let getLayoutRaw =
          buffer.pop(UnsafeRawPointer.self)
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
        size += (addedSize + alignmentMask) & ~alignmentMask
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

    // Break if this is the last component.
    if buffer.data.count == 0 { break }

    // Pop the type accessor reference.
    _ = buffer.popRaw(size: MemoryLayout<Int>.size,
                      alignment: MemoryLayout<Int>.alignment)
  }

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
          size: size, alignmentMask: alignmentMask)
}

@inlinable // FIXME(sil-serialize-all)
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


    func tryToResolveOffset() {
      if header.payload == RawKeyPathComponent.Header.unresolvedFieldOffsetPayload {
        // Look up offset in type metadata. The value in the pattern is the
        // offset within the metadata object.
        let metadataPtr = unsafeBitCast(base, to: UnsafeRawPointer.self)
        let offsetOfOffset = patternBuffer.pop(UInt32.self)

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

      // The ID may need resolution if the property is keyed by a selector.
      var newHeader = header
      var id = patternBuffer.pop(Int.self)
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
      let getter = patternBuffer.pop(UnsafeRawPointer.self)
      pushDest(getter)
      if header.payload & RawKeyPathComponent.Header.computedSettableFlag != 0{
        let setter = patternBuffer.pop(UnsafeRawPointer.self)
        pushDest(setter)
      }
      // Carry over the arguments.
      if header.payload
          & RawKeyPathComponent.Header.computedHasArgumentsFlag != 0 {
        let getLayoutRaw = patternBuffer.pop(UnsafeRawPointer.self)
        let getLayout = unsafeBitCast(getLayoutRaw,
          to: RawKeyPathComponent.ComputedArgumentLayoutFn.self)

        let witnesses = patternBuffer.pop(
          UnsafePointer<ComputedArgumentWitnesses>.self)

        if let _ = witnesses.pointee.destroy {
          isTrivial = false
        }

        let initializerRaw = patternBuffer.pop(UnsafeRawPointer.self)
        let initializer = unsafeBitCast(initializerRaw,
          to: RawKeyPathComponent.ComputedArgumentInitializerFn.self)

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
    }

    // Break if this is the last component.
    if patternBuffer.data.count == 0 { break }

    // Resolve the component type.
    let componentTyAccessor = patternBuffer.pop(MetadataAccessor.self)
    base = unsafeBitCast(componentTyAccessor(arguments), to: Any.Type.self)
    pushDest(base)
    previousComponentAddr = componentAddr
  }

  // We should have traversed both buffers.
  _sanityCheck(patternBuffer.data.isEmpty && destData.count == 0)

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
