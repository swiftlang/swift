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

// This file contains the subset of the key path runtime implementation that
// handles instantiation of key path objects from key path patterns. It is
// compiled into both the Swift standard library and into the runtime
// compatibility backward deployment libraries, so that updates to key path
// instantiation can be deployed to operating systems with older Swift
// runtimes.

#if COMPATIBILITY_LIBRARY
import Swift
import Darwin

// Reimplement some stdlib-private helpers for use in the compat library.
#if INTERNAL_CHECKS_ENABLED

func _internalInvariant(_ condition: @autoclosure () -> Bool, 
                        _ reason: StaticString = "") {
  precondition(condition(), "\(reason)")
}
func _internalInvariantFailure(_ reason: StaticString = "") -> Never {
  preconditionFailure("\(reason)")
}
func _precondition(_ condition: @autoclosure () -> Bool,
                   _ reason: StaticString = "") {
  precondition(condition(), "\(reason)")
}

#else // INTERNAL_CHECKS_ENABLED

func _internalInvariant(_ condition: @autoclosure () -> Bool,
                        _ reason: StaticString = "") {
}
func _internalInvariantFailure(_ reason: StaticString = "") -> Never {
  preconditionFailure(reason)
}
func _precondition(_ condition: @autoclosure () -> Bool,
                   _ reason: StaticString) {
}

#endif // INTERNAL_CHECKS_ENABLED

// _memcpy
func _memcpy(dest: UnsafeMutableRawPointer,
             src: UnsafeRawPointer,
             size: UInt) {
  memcpy(dest, src, Int(size))
}

extension MemoryLayout {
  static func _roundingUpToAlignment(_ value: Int) -> Int {
    return (value + _alignmentMask) & ~_alignmentMask
  }

  static func _roundingUpBaseToAlignment(_ value: UnsafeRawBufferPointer)
      -> UnsafeRawBufferPointer {
    let baseAddressBits = Int(bitPattern: value.baseAddress)
    var misalignment = baseAddressBits & _alignmentMask
    if misalignment != 0 {
      misalignment = _alignmentMask & -misalignment
      return UnsafeRawBufferPointer(
        start: UnsafeRawPointer(bitPattern: baseAddressBits + misalignment),
        count: value.count - misalignment)
    }
    return value
  }

  static var _alignmentMask: Int {
    return alignment - 1
  }
}

extension AnyKeyPath {
  var _kvcKeyPathStringOffset: Int {

#if COMPATIBILITY_LIBRARY_50
    // In the 5.0 runtime, the _kvcKeyPathStringPtr field is the first field
    // after the object header.
    return MemoryLayout<Int>.size * 2
#else
 #error("unimplemented compatibility library version")
#endif

  }

  var _kvcKeyPathStringPtrPtr: UnsafeMutablePointer<UnsafePointer<CChar>?> {
    return (unsafeBitCast(self, to: UnsafeMutableRawPointer.self)
      + _kvcKeyPathStringOffset)
      .assumingMemoryBound(to: UnsafePointer<CChar>?.self)
  }

  var _kvcKeyPathStringPtr: UnsafePointer<CChar>? {
    _read {
      yield _kvcKeyPathStringPtrPtr.pointee
    }
    _modify {
      yield &_kvcKeyPathStringPtrPtr.pointee
    }
  }

#if COMPATIBILITY_LIBRARY_50
  @_silgen_name("swift50override_keyPath_create")
  internal static func _create(
    capacityInBytes bytes: Int,
    initializedBy body: (UnsafeMutableRawBufferPointer) -> Void
  ) -> Self
#else
 #error("unimplemented compatibility library version")
#endif
}

#endif // COMPATIBILITY_LIBRARY

import SwiftShims

internal enum KeyPathKind { case readOnly, value, reference }

internal enum KeyPathComputedIDKind {
  case pointer
  case storedPropertyIndex
  case vtableOffset
}

internal enum KeyPathComputedIDResolution {
  case resolved
  case indirectPointer
  case functionCall
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
      _internalInvariant(size <= Int(Header.sizeMask), "key path too big")
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
    internal static var storedMutableFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_StoredMutableFlag
    }
    internal static var storedOffsetPayloadMask: UInt32 {
      return _SwiftKeyPathComponentHeader_StoredOffsetPayloadMask
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

    internal var isStoredMutable: Bool {
      _internalInvariant(kind == .struct || kind == .class)
      return _value & Header.storedMutableFlag != 0
    }

    internal static var computedMutatingFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedMutatingFlag
    }
    internal var isComputedMutating: Bool {
      _internalInvariant(kind == .computed)
      return _value & Header.computedMutatingFlag != 0
    }

    internal static var computedSettableFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedSettableFlag
    }
    internal var isComputedSettable: Bool {
      _internalInvariant(kind == .computed)
      return _value & Header.computedSettableFlag != 0
    }

    internal static var computedIDByStoredPropertyFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDByStoredPropertyFlag
    }
    internal static var computedIDByVTableOffsetFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDByVTableOffsetFlag
    }
    internal var computedIDKind: KeyPathComputedIDKind {
      let storedProperty = _value & Header.computedIDByStoredPropertyFlag != 0
      let vtableOffset = _value & Header.computedIDByVTableOffsetFlag != 0

      switch (storedProperty, vtableOffset) {
      case (true, true):
        _internalInvariantFailure("not allowed")
      case (true, false):
        return .storedPropertyIndex
      case (false, true):
        return .vtableOffset
      case (false, false):
        return .pointer
      }
    }

    internal static var computedHasArgumentsFlag: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedHasArgumentsFlag
    }
    internal var hasComputedArguments: Bool {
      _internalInvariant(kind == .computed)
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
        _internalInvariant(kind == .computed)
        return
          _value & Header.computedInstantiatedFromExternalWithArgumentsFlag != 0
      }
      set {
        _internalInvariant(kind == .computed)
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
    internal static var computedIDUnresolvedFunctionCall: UInt32 {
      return _SwiftKeyPathComponentHeader_ComputedIDUnresolvedFunctionCall
    }
    internal var computedIDResolution: KeyPathComputedIDResolution {
      switch payload & Header.computedIDResolutionMask {
      case Header.computedIDResolved:
        return .resolved
      case Header.computedIDUnresolvedIndirectPointer:
        return .indirectPointer
      case Header.computedIDUnresolvedFunctionCall:
        return .functionCall
      default:
        _internalInvariantFailure("invalid key path resolution")
      }
    }
    
    internal var _value: UInt32
    
    internal var discriminator: UInt32 {
      get {
        return (_value & Header.discriminatorMask) >> Header.discriminatorShift
      }
      set {
        let shifted = newValue << Header.discriminatorShift
        _internalInvariant(shifted & Header.discriminatorMask == shifted,
                     "discriminator doesn't fit")
        _value = _value & ~Header.discriminatorMask | shifted
      }
    }
    internal var payload: UInt32 {
      get {
        return _value & Header.payloadMask
      }
      set {
        _internalInvariant(newValue & Header.payloadMask == newValue,
                     "payload too big")
        _value = _value & ~Header.payloadMask | newValue
      }
    }
    internal var storedOffsetPayload: UInt32 {
      get {
        _internalInvariant(kind == .struct || kind == .class,
                     "not a stored component")
        return _value & Header.storedOffsetPayloadMask
      }
      set {
        _internalInvariant(kind == .struct || kind == .class,
                     "not a stored component")
        _internalInvariant(newValue & Header.storedOffsetPayloadMask == newValue,
                     "payload too big")
        _value = _value & ~Header.storedOffsetPayloadMask | newValue
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
        _internalInvariantFailure("invalid header")
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
        if storedOffsetPayload == Header.unresolvedFieldOffsetPayload
           || storedOffsetPayload == Header.outOfLineOffsetPayload
           || storedOffsetPayload == Header.unresolvedIndirectOffsetPayload {
          // A 32-bit offset is stored in the body.
          return MemoryLayout<UInt32>.size
        }
        // Otherwise, there's no body.
        return 0

      case .external:
        // The body holds a pointer to the external property descriptor,
        // and some number of substitution arguments, the count of which is
        // in the payload.
        return 4 * (1 + Int(payload))

      case .computed:
        // The body holds at minimum the id and getter.
        var size = 8
        // If settable, it also holds the setter.
        if isComputedSettable {
          size += 4
        }
        // If there are arguments, there's also a layout function,
        // witness table, and initializer function.
        // Property descriptors never carry argument information, though.
        if !forPropertyDescriptor && hasComputedArguments {
          size += 12
        }

        return size

      case .optionalForce, .optionalChain, .optionalWrap:
        // Otherwise, there's no body.
        return 0
      }
    }

    init(discriminator: UInt32, payload: UInt32) {
      _value = 0
      self.discriminator = discriminator
      self.payload = payload
    }

    init(optionalForce: ()) {
      self.init(discriminator: Header.optionalTag,
                payload: Header.optionalForcePayload)
    }

    init(optionalWrap: ()) {
      self.init(discriminator: Header.optionalTag,
                payload: Header.optionalWrapPayload)
    }

    init(optionalChain: ()) {
      self.init(discriminator: Header.optionalTag,
                payload: Header.optionalChainPayload)
    }

    init(stored kind: KeyPathStructOrClass,
         mutable: Bool,
         inlineOffset: UInt32) {
      let discriminator: UInt32
      switch kind {
      case .struct: discriminator = Header.structTag
      case .class: discriminator = Header.classTag
      }

      _internalInvariant(inlineOffset <= Header.maximumOffsetPayload)
      let payload = inlineOffset
        | (mutable ? Header.storedMutableFlag : 0)
      self.init(discriminator: discriminator,
                payload: payload)
    }

    init(storedWithOutOfLineOffset kind: KeyPathStructOrClass,
         mutable: Bool) {
      let discriminator: UInt32
      switch kind {
      case .struct: discriminator = Header.structTag
      case .class: discriminator = Header.classTag
      }

      let payload = Header.outOfLineOffsetPayload
        | (mutable ? Header.storedMutableFlag : 0)

      self.init(discriminator: discriminator,
                payload: payload)
    }

    init(computedWithIDKind kind: KeyPathComputedIDKind,
         mutating: Bool,
         settable: Bool,
         hasArguments: Bool,
         instantiatedFromExternalWithArguments: Bool) {
      let discriminator = Header.computedTag
      var payload =
          (mutating ? Header.computedMutatingFlag : 0)
        | (settable ? Header.computedSettableFlag : 0)
        | (hasArguments ? Header.computedHasArgumentsFlag : 0)
        | (instantiatedFromExternalWithArguments
             ? Header.computedInstantiatedFromExternalWithArgumentsFlag : 0)
      switch kind {
      case .pointer:
        break
      case .storedPropertyIndex:
        payload |= Header.computedIDByStoredPropertyFlag
      case .vtableOffset:
        payload |= Header.computedIDByVTableOffsetFlag
      }
      self.init(discriminator: discriminator,
                payload: payload)
    }
  }

  internal var _computedIDValue: Int {
    _internalInvariant(header.kind == .computed,
                 "not a computed property")
    return body.load(fromByteOffset: Header.pointerAlignmentSkew,
                     as: Int.self)
  }

  internal var _computedID: ComputedPropertyID {
    _internalInvariant(header.kind == .computed,
                 "not a computed property")

    return ComputedPropertyID(
      value: _computedIDValue,
      kind: header.computedIDKind)
  }

  internal var _computedGetter: UnsafeRawPointer {
    _internalInvariant(header.kind == .computed,
                 "not a computed property")

    return body.load(
      fromByteOffset: Header.pointerAlignmentSkew + MemoryLayout<Int>.size,
      as: UnsafeRawPointer.self)
  }

  internal var _computedSetter: UnsafeRawPointer {
    _internalInvariant(header.isComputedSettable,
                 "not a settable property")

    return body.load(
      fromByteOffset: Header.pointerAlignmentSkew + MemoryLayout<Int>.size * 2,
      as: UnsafeRawPointer.self)
  }

  internal var _computedArgumentHeaderPointer: UnsafeRawPointer {
    _internalInvariant(header.hasComputedArguments, "no arguments")

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

}

internal func _pop<T>(from: inout UnsafeRawBufferPointer,
                      as type: T.Type) -> T {
  let buffer = _pop(from: &from, as: type, count: 1)
  return buffer.baseAddress.unsafelyUnwrapped.pointee
}
internal func _pop<T>(from: inout UnsafeRawBufferPointer,
                      as: T.Type,
                      count: Int) -> UnsafeBufferPointer<T> {
  _internalInvariant(_isPOD(T.self), "should be POD")
  from = MemoryLayout<T>._roundingUpBaseToAlignment(from)
  let byteCount = MemoryLayout<T>.stride * count
  let result = UnsafeBufferPointer(
    start: from.baseAddress.unsafelyUnwrapped.assumingMemoryBound(to: T.self),
    count: count)

  from = UnsafeRawBufferPointer(
    start: from.baseAddress.unsafelyUnwrapped + byteCount,
    count: from.count - byteCount)
  return result
}
  
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
  internal var value: Int
  internal var kind: KeyPathComputedIDKind

  internal static func ==(
    x: ComputedPropertyID, y: ComputedPropertyID
  ) -> Bool {
    return x.value == y.value
      && x.kind == y.kind
  }

  internal func hash(into hasher: inout Hasher) {
    hasher.combine(value)
    hasher.combine(kind)
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

internal typealias KeyPathComputedArgumentLayoutFn = @convention(thin)
  (_ patternArguments: UnsafeRawPointer?) -> (size: Int, alignmentMask: Int)
internal typealias KeyPathComputedArgumentInitializerFn = @convention(thin)
  (_ patternArguments: UnsafeRawPointer?,
   _ instanceArguments: UnsafeMutableRawPointer) -> ()

// The distance in bytes from the address point of a KeyPath object to its
// buffer header. Includes the size of the Swift heap object header and the
// pointer to the KVC string.

internal var keyPathObjectHeaderSize: Int {
  return MemoryLayout<HeapObject>.size + MemoryLayout<Int>.size
}

internal var keyPathPatternHeaderSize: Int {
  return 16
}

// Runtime entry point to instantiate a key path object.
// Note that this has a compatibility override shim in the runtime so that
// future compilers can backward-deploy support for instantiating new key path
// pattern features.
internal func _swift_getKeyPath(pattern: UnsafeMutableRawPointer,
                                arguments: UnsafeRawPointer)
    -> UnsafeRawPointer {
  // The key path pattern is laid out like a key path object, with a few
  // modifications:
  // - Pointers in the instantiated object are compressed into 32-bit
  //   relative offsets in the pattern.
  // - The pattern begins with a field that's either zero, for a pattern that
  //   depends on instantiation arguments, or that's a relative reference to
  //   a global mutable pointer variable, which can be initialized to a single
  //   shared instantiation of this pattern.
  // - Instead of the two-word object header with isa and refcount, two
  //   pointers to metadata accessors are provided for the root and leaf
  //   value types of the key path.
  // - Components may have unresolved forms that require instantiation.
  // - Type metadata and protocol conformance pointers are replaced with
  //   relative-referenced accessor functions that instantiate the
  //   needed generic argument when called.
  //
  // The pattern never precomputes the capabilities of the key path (readonly/
  // writable/reference-writable), nor does it encode the reference prefix.
  // These are resolved dynamically, so that they always reflect the dynamic
  // capability of the properties involved.

  let oncePtrPtr = pattern
  let patternPtr = pattern.advanced(by: 4)

  let bufferHeader = patternPtr.load(fromByteOffset: keyPathPatternHeaderSize,
                                     as: KeyPathBuffer.Header.self)
  bufferHeader.validateReservedBits()

  // If the first word is nonzero, it relative-references a cache variable
  // we can use to reference a single shared instantiation of this key path.
  let oncePtrOffset = oncePtrPtr.load(as: Int32.self)
  let oncePtr: UnsafeRawPointer?
  if oncePtrOffset != 0 {
    let theOncePtr = _resolveRelativeAddress(oncePtrPtr, oncePtrOffset)
    oncePtr = theOncePtr

    // See whether we already instantiated this key path.
    // This is a non-atomic load because the instantiated pointer will be
    // written with a release barrier, and loads of the instantiated key path
    // ought to carry a dependency through this loaded pointer.
    let existingInstance = theOncePtr.load(as: UnsafeRawPointer?.self)
    
    if let existingInstance = existingInstance {
      // Return the instantiated object at +1.
      let object = Unmanaged<AnyKeyPath>.fromOpaque(existingInstance)
      // TODO: This retain will be unnecessary once we support global objects
      // with inert refcounting.
      _ = object.retain()
      return existingInstance
    }
  } else {
    oncePtr = nil
  }

  // Instantiate a new key path object modeled on the pattern.
  // Do a pass to determine the class of the key path we'll be instantiating
  // and how much space we'll need for it.
  let (keyPathClass, rootType, size, _)
    = _getKeyPathClassAndInstanceSizeFromPattern(patternPtr, arguments)

  // Allocate the instance.
  let instance = keyPathClass._create(capacityInBytes: size) { instanceData in
    // Instantiate the pattern into the instance.
    _instantiateKeyPathBuffer(patternPtr, instanceData, rootType, arguments)
  }

  // Adopt the KVC string from the pattern.
  let kvcStringBase = patternPtr.advanced(by: 12)
  let kvcStringOffset = kvcStringBase.load(as: Int32.self)

  if kvcStringOffset == 0 {
    // Null pointer.
    instance._kvcKeyPathStringPtr = nil
  } else {
    let kvcStringPtr = _resolveRelativeAddress(kvcStringBase, kvcStringOffset)
    instance._kvcKeyPathStringPtr =
      kvcStringPtr.assumingMemoryBound(to: CChar.self)
  }

  // If we can cache this instance as a shared instance, do so.
  if let oncePtr = oncePtr {
    // Try to replace a null pointer in the cache variable with the instance
    // pointer.
    let instancePtr = Unmanaged.passRetained(instance)

    while true {
      let (oldValue, won) = Builtin.cmpxchg_seqcst_seqcst_Word(
        oncePtr._rawValue,
        0._builtinWordValue,
        UInt(bitPattern: instancePtr.toOpaque())._builtinWordValue)

      // If the exchange succeeds, then the instance we formed is the canonical
      // one.
      if unsafeBitCast(won, to: Bool.self) {
        break
      }

      // Otherwise, someone raced with us to instantiate the key path pattern
      // and won. Their instance should be just as good as ours, so we can take
      // that one and let ours get deallocated.
      if let existingInstance = UnsafeRawPointer(bitPattern: Int(oldValue)) {
        // Return the instantiated object at +1.
        let object = Unmanaged<AnyKeyPath>.fromOpaque(existingInstance)
        // TODO: This retain will be unnecessary once we support global objects
        // with inert refcounting.
        _ = object.retain()
        // Release the instance we created.
        instancePtr.release()
        return existingInstance
      } else {
        // Try the cmpxchg again if it spuriously failed.
        continue
      }
    }
  }

  return UnsafeRawPointer(Unmanaged.passRetained(instance).toOpaque())
}

#if COMPATIBILITY_LIBRARY
 #if COMPATIBILITY_LIBRARY_50

@_cdecl("swift50override_getKeyPath")
internal func swift50override_getKeyPath(
  pattern: UnsafeMutableRawPointer,
  arguments: UnsafeRawPointer,
  originalImplementation:
    @convention(c) (UnsafeMutableRawPointer, UnsafeRawPointer) -> UnsafeRawPointer
) -> UnsafeRawPointer {
  return _swift_getKeyPath(pattern: pattern, arguments: arguments)
}

 #else
  #error("unimplemented compatibility library version")
 #endif
#else

@_cdecl("swift_getKeyPathImpl")
internal func swift_getKeyPathImpl(
  pattern: UnsafeMutableRawPointer,
  arguments: UnsafeRawPointer
) -> UnsafeRawPointer {
  return _swift_getKeyPath(pattern: pattern, arguments: arguments)
}

#endif

// A reference to metadata, which is a pointer to a mangled name.
internal typealias MetadataReference = UnsafeRawPointer

// Determine the length of the given mangled name.
internal func _getSymbolicMangledNameLength(_ base: UnsafeRawPointer) -> Int {
  var end = base
  while let current = Optional(end.load(as: UInt8.self)), current != 0 {
    // Skip the current character
    end = end + 1

    // Skip over a symbolic reference
    if current >= 0x1 && current <= 0x17 {
      end += 4
    } else if current >= 0x18 && current <= 0x1F {
      end += MemoryLayout<Int>.size
    }
  }

  return end - base
}

// Resolve a mangled name in a generic environment, described by either a
// flat GenericEnvironment * (if the bottom tag bit is 0) or possibly-nested
// ContextDescriptor * (if the bottom tag bit is 1)
internal func _getTypeByMangledNameInEnvironmentOrContext(
  _ name: UnsafePointer<UInt8>,
  _ nameLength: UInt,
  genericEnvironmentOrContext: UnsafeRawPointer?,
  genericArguments: UnsafeRawPointer?)
  -> Any.Type? {

  let taggedPointer = UInt(bitPattern: genericEnvironmentOrContext)
  if taggedPointer & 1 == 0 {
    return _getTypeByMangledNameInEnvironment(name, nameLength,
                      genericEnvironment: genericEnvironmentOrContext,
                      genericArguments: genericArguments)
  } else {
    let context = UnsafeRawPointer(bitPattern: taggedPointer & ~1)
    return _getTypeByMangledNameInContext(name, nameLength,
                      genericContext: context,
                      genericArguments: genericArguments)
  }
}

// Resolve the given generic argument reference to a generic argument.
internal func _resolveKeyPathGenericArgReference(
    _ reference: UnsafeRawPointer,
    genericEnvironment: UnsafeRawPointer?,
    arguments: UnsafeRawPointer?)
    -> UnsafeRawPointer {
  // If the low bit is clear, it's a direct reference to the argument.
  if (UInt(bitPattern: reference) & 0x01 == 0) {
    return reference
  }

  // Adjust the reference.
  let referenceStart = reference - 1

  let nameLength = _getSymbolicMangledNameLength(referenceStart)
  let namePtr = referenceStart.bindMemory(to: UInt8.self,
                                          capacity: nameLength + 1)
  // FIXME: Could extract this information from the mangled name.
  guard let result =
    _getTypeByMangledNameInEnvironmentOrContext(namePtr, UInt(nameLength),
                         genericEnvironmentOrContext: genericEnvironment,
                         genericArguments: arguments)
  else {
    let nameStr = String(
      decoding: UnsafeBufferPointer(start: namePtr, count: nameLength),
      as: UTF8.self
    )
    fatalError("could not demangle keypath type from '\(nameStr)'")
  }

  return unsafeBitCast(result, to: UnsafeRawPointer.self)
}

// Resolve the given metadata reference to (type) metadata.
internal func _resolveKeyPathMetadataReference(
    _ reference: UnsafeRawPointer,
    genericEnvironment: UnsafeRawPointer?,
    arguments: UnsafeRawPointer?)
    -> Any.Type {
  return unsafeBitCast(
           _resolveKeyPathGenericArgReference(
             reference,
             genericEnvironment: genericEnvironment,
             arguments: arguments),
           to: Any.Type.self)
}

internal enum KeyPathStructOrClass {
  case `struct`, `class`
}
internal enum KeyPathPatternStoredOffset {
  case inline(UInt32)
  case outOfLine(UInt32)
  case unresolvedFieldOffset(UInt32)
  case unresolvedIndirectOffset(UnsafePointer<UInt32>)
}
internal struct KeyPathPatternComputedArguments {
  var getLayout: KeyPathComputedArgumentLayoutFn
  var witnesses: UnsafePointer<ComputedArgumentWitnesses>
  var initializer: KeyPathComputedArgumentInitializerFn
}
internal protocol KeyPathPatternVisitor {
  mutating func visitHeader(genericEnvironment: UnsafeRawPointer?,
                            rootMetadataRef: MetadataReference,
                            leafMetadataRef: MetadataReference,
                            kvcCompatibilityString: UnsafeRawPointer?)
  mutating func visitStoredComponent(kind: KeyPathStructOrClass,
                                     mutable: Bool,
                                     offset: KeyPathPatternStoredOffset)
  mutating func visitComputedComponent(mutating: Bool,
                                       idKind: KeyPathComputedIDKind,
                                       idResolution: KeyPathComputedIDResolution,
                                       idValueBase: UnsafeRawPointer,
                                       idValue: Int32,
                                       getter: UnsafeRawPointer,
                                       setter: UnsafeRawPointer?,
                                       arguments: KeyPathPatternComputedArguments?,
                                       externalArgs: UnsafeBufferPointer<Int32>?)
  mutating func visitOptionalChainComponent()
  mutating func visitOptionalForceComponent()
  mutating func visitOptionalWrapComponent()

  mutating func visitIntermediateComponentType(metadataRef: MetadataReference)

  mutating func finish()
}

internal func _resolveRelativeAddress(_ base: UnsafeRawPointer,
                                      _ offset: Int32) -> UnsafeRawPointer {
  // Sign-extend the offset to pointer width and add with wrap on overflow.
  return UnsafeRawPointer(bitPattern: Int(bitPattern: base) &+ Int(offset))
    .unsafelyUnwrapped
}
internal func _resolveRelativeIndirectableAddress(_ base: UnsafeRawPointer,
                                                  _ offset: Int32)
    -> UnsafeRawPointer {
  // Low bit indicates whether the reference is indirected or not.
  if offset & 1 != 0 {
    let ptrToPtr = _resolveRelativeAddress(base, offset - 1)
    return ptrToPtr.load(as: UnsafeRawPointer.self)
  }
  return _resolveRelativeAddress(base, offset)
}
internal func _loadRelativeAddress<T>(at: UnsafeRawPointer,
                                      fromByteOffset: Int = 0,
                                      as: T.Type) -> T {
  let offset = at.load(fromByteOffset: fromByteOffset, as: Int32.self)
  return unsafeBitCast(_resolveRelativeAddress(at + fromByteOffset, offset),
                       to: T.self)
}
internal func _walkKeyPathPattern<W: KeyPathPatternVisitor>(
                                  _ pattern: UnsafeRawPointer,
                                  walker: inout W) {
  // Visit the header.
  let genericEnvironment = _loadRelativeAddress(at: pattern,
                                                as: UnsafeRawPointer.self)
  let rootMetadataRef = _loadRelativeAddress(at: pattern, fromByteOffset: 4,
                                             as: MetadataReference.self)
  let leafMetadataRef = _loadRelativeAddress(at: pattern, fromByteOffset: 8,
                                             as: MetadataReference.self)
  let kvcString = _loadRelativeAddress(at: pattern, fromByteOffset: 12,
                                       as: UnsafeRawPointer.self)

  walker.visitHeader(genericEnvironment: genericEnvironment,
                     rootMetadataRef: rootMetadataRef,
                     leafMetadataRef: leafMetadataRef,
                     kvcCompatibilityString: kvcString)

  func visitStored(header: RawKeyPathComponent.Header,
                   componentBuffer: inout UnsafeRawBufferPointer) {
    // Decode a stored property. A small offset may be stored inline in the
    // header word, or else be stored out-of-line, or need instantiation of some
    // kind.
    let offset: KeyPathPatternStoredOffset
    switch header.storedOffsetPayload {
    case RawKeyPathComponent.Header.outOfLineOffsetPayload:
      offset = .outOfLine(_pop(from: &componentBuffer,
                               as: UInt32.self))
    case RawKeyPathComponent.Header.unresolvedFieldOffsetPayload:
      offset = .unresolvedFieldOffset(_pop(from: &componentBuffer,
                                           as: UInt32.self))
    case RawKeyPathComponent.Header.unresolvedIndirectOffsetPayload:
      let base = componentBuffer.baseAddress.unsafelyUnwrapped
      let relativeOffset = _pop(from: &componentBuffer,
                                as: Int32.self)
      let ptr = _resolveRelativeIndirectableAddress(base, relativeOffset)
      offset = .unresolvedIndirectOffset(
                                       ptr.assumingMemoryBound(to: UInt32.self))
    default:
      offset = .inline(header.storedOffsetPayload)
    }
    let kind: KeyPathStructOrClass = header.kind == .struct 
      ? .struct : .class
    walker.visitStoredComponent(kind: kind,
                                mutable: header.isStoredMutable,
                                offset: offset)
  }

  func popComputedAccessors(header: RawKeyPathComponent.Header,
                            componentBuffer: inout UnsafeRawBufferPointer)
      -> (idValueBase: UnsafeRawPointer,
          idValue: Int32,
          getter: UnsafeRawPointer,
          setter: UnsafeRawPointer?) {
    let idValueBase = componentBuffer.baseAddress.unsafelyUnwrapped
    let idValue = _pop(from: &componentBuffer, as: Int32.self)
    let getterBase = componentBuffer.baseAddress.unsafelyUnwrapped
    let getterRef = _pop(from: &componentBuffer, as: Int32.self)
    let getter = _resolveRelativeAddress(getterBase, getterRef)
    let setter: UnsafeRawPointer?
    if header.isComputedSettable {
      let setterBase = componentBuffer.baseAddress.unsafelyUnwrapped
      let setterRef = _pop(from: &componentBuffer, as: Int32.self)
      setter = _resolveRelativeAddress(setterBase, setterRef)
    } else {
      setter = nil
    }
    return (idValueBase: idValueBase, idValue: idValue,
            getter: getter, setter: setter)
  }

  func popComputedArguments(header: RawKeyPathComponent.Header,
                            componentBuffer: inout UnsafeRawBufferPointer)
      -> KeyPathPatternComputedArguments? {
    if header.hasComputedArguments {
      let getLayoutBase = componentBuffer.baseAddress.unsafelyUnwrapped
      let getLayoutRef = _pop(from: &componentBuffer, as: Int32.self)
      let getLayoutRaw = _resolveRelativeAddress(getLayoutBase, getLayoutRef)
      let getLayout = unsafeBitCast(getLayoutRaw,
                                    to: KeyPathComputedArgumentLayoutFn.self)

      let witnessesBase = componentBuffer.baseAddress.unsafelyUnwrapped
      let witnessesRef = _pop(from: &componentBuffer, as: Int32.self)
      let witnesses: UnsafeRawPointer
      if witnessesRef == 0 {
        witnesses = __swift_keyPathGenericWitnessTable_addr()
      } else {
        witnesses = _resolveRelativeAddress(witnessesBase, witnessesRef)
      }

      let initializerBase = componentBuffer.baseAddress.unsafelyUnwrapped
      let initializerRef = _pop(from: &componentBuffer, as: Int32.self)
      let initializerRaw = _resolveRelativeAddress(initializerBase,
                                                   initializerRef)
      let initializer = unsafeBitCast(initializerRaw,
                                  to: KeyPathComputedArgumentInitializerFn.self)

      return KeyPathPatternComputedArguments(getLayout: getLayout,
        witnesses:
              witnesses.assumingMemoryBound(to: ComputedArgumentWitnesses.self),
        initializer: initializer)
    } else {
      return nil
    }
  }

  // We declare this down here to avoid the temptation to use it within
  // the functions above.
  let bufferPtr = pattern.advanced(by: keyPathPatternHeaderSize)
  let bufferHeader = bufferPtr.load(as: KeyPathBuffer.Header.self)
  var buffer = UnsafeRawBufferPointer(start: bufferPtr + 4,
                                      count: bufferHeader.size)

  while !buffer.isEmpty {
    let header = _pop(from: &buffer,
                      as: RawKeyPathComponent.Header.self)

    // Ensure that we pop an amount of data consistent with what
    // RawKeyPathComponent.Header.patternComponentBodySize computes.
    var bufferSizeBefore = 0
    var expectedPop = 0

    _internalInvariant({
      bufferSizeBefore = buffer.count
      expectedPop = header.patternComponentBodySize
      return true
    }())

    switch header.kind {
    case .class, .struct:
      visitStored(header: header, componentBuffer: &buffer)
    case .computed:
      let (idValueBase, idValue, getter, setter)
        = popComputedAccessors(header: header,
                               componentBuffer: &buffer)

      // If there are arguments, gather those too.
      let arguments = popComputedArguments(header: header,
                                           componentBuffer: &buffer)

      walker.visitComputedComponent(mutating: header.isComputedMutating,
                                    idKind: header.computedIDKind,
                                    idResolution: header.computedIDResolution,
                                    idValueBase: idValueBase,
                                    idValue: idValue,
                                    getter: getter,
                                    setter: setter,
                                    arguments: arguments,
                                    externalArgs: nil)

    case .optionalChain:
      walker.visitOptionalChainComponent()
    case .optionalWrap:
      walker.visitOptionalWrapComponent()
    case .optionalForce:
      walker.visitOptionalForceComponent()
    case .external:
      // Look at the external property descriptor to see if we should take it
      // over the component given in the pattern.
      let genericParamCount = Int(header.payload)
      let descriptorBase = buffer.baseAddress.unsafelyUnwrapped
      let descriptorOffset = _pop(from: &buffer,
                                  as: Int32.self)
      let descriptor =
        _resolveRelativeIndirectableAddress(descriptorBase, descriptorOffset)
      let descriptorHeader =
        descriptor.load(as: RawKeyPathComponent.Header.self)
      if descriptorHeader.isTrivialPropertyDescriptor {
        // If the descriptor is trivial, then use the local candidate.
        // Skip the external generic parameter accessors to get to it.
        _ = _pop(from: &buffer, as: Int32.self, count: genericParamCount)
        continue
      }
      
      // Grab the generic parameter accessors to pass to the external component.
      let externalArgs = _pop(from: &buffer, as: Int32.self,
                              count: genericParamCount)

      // Grab the header for the local candidate in case we need it for
      // a computed property.
      let localCandidateHeader = _pop(from: &buffer,
                                      as: RawKeyPathComponent.Header.self)
      let localCandidateSize = localCandidateHeader.patternComponentBodySize
      _internalInvariant({
        expectedPop += localCandidateSize + 4
        return true
      }())

      let descriptorSize = descriptorHeader.propertyDescriptorBodySize
      var descriptorBuffer = UnsafeRawBufferPointer(start: descriptor + 4,
                                                    count: descriptorSize)

      // Look at what kind of component the external property has.
      switch descriptorHeader.kind {
      case .struct, .class:
        // A stored component. We can instantiate it
        // without help from the local candidate.
        _ = _pop(from: &buffer, as: UInt8.self, count: localCandidateSize)

        visitStored(header: descriptorHeader,
                    componentBuffer: &descriptorBuffer)
        
      case .computed:
        // A computed component. The accessors come from the descriptor.
        let (idValueBase, idValue, getter, setter)
          = popComputedAccessors(header: descriptorHeader,
                                 componentBuffer: &descriptorBuffer)
        
        // Get the arguments from the external descriptor and/or local candidate
        // component.
        let arguments: KeyPathPatternComputedArguments?
        if localCandidateHeader.kind == .computed
            && localCandidateHeader.hasComputedArguments {
          // If both have arguments, then we have to build a bit of a chimera.
          // The canonical identity and accessors come from the descriptor,
          // but the argument equality/hash handling is still as described
          // in the local candidate.
          // We don't need the local candidate's accessors.
          _ = popComputedAccessors(header: localCandidateHeader,
                                   componentBuffer: &buffer)
          // We do need the local arguments.
          arguments = popComputedArguments(header: localCandidateHeader,
                                           componentBuffer: &buffer)
        } else {
          // If the local candidate doesn't have arguments, we don't need
          // anything from it at all.
          _ = _pop(from: &buffer, as: UInt8.self, count: localCandidateSize)
          arguments = nil
        }

        walker.visitComputedComponent(
          mutating: descriptorHeader.isComputedMutating,
          idKind: descriptorHeader.computedIDKind,
          idResolution: descriptorHeader.computedIDResolution,
          idValueBase: idValueBase,
          idValue: idValue,
          getter: getter,
          setter: setter,
          arguments: arguments,
          externalArgs: genericParamCount > 0 ? externalArgs : nil)
      case .optionalChain, .optionalWrap, .optionalForce, .external:
        _internalInvariantFailure("not possible for property descriptor")
      }
    }

    // Check that we consumed the expected amount of data from the pattern.
    _internalInvariant(
      {
        // Round the amount of data we read up to alignment.
        let popped = MemoryLayout<Int32>._roundingUpToAlignment(
           bufferSizeBefore - buffer.count)
        return expectedPop == popped
      }(),
      """
      component size consumed during pattern walk does not match \
      component size returned by patternComponentBodySize
      """)

    // Break if this is the last component.
    if buffer.isEmpty { break }

    // Otherwise, pop the intermediate component type accessor and
    // go around again.
    let componentTypeBase = buffer.baseAddress.unsafelyUnwrapped
    let componentTypeOffset = _pop(from: &buffer, as: Int32.self)
    let componentTypeRef = _resolveRelativeAddress(componentTypeBase,
                                                   componentTypeOffset)
    walker.visitIntermediateComponentType(metadataRef: componentTypeRef)
    _internalInvariant(!buffer.isEmpty)
  }

  // We should have walked the entire pattern.
  _internalInvariant(buffer.isEmpty, "did not walk entire pattern buffer")
  walker.finish()
}

internal struct GetKeyPathClassAndInstanceSizeFromPattern
    : KeyPathPatternVisitor {
  var size: Int = MemoryLayout<Int>.size // start with one word for the header
  var capability: KeyPathKind = .value
  var didChain: Bool = false
  var root: Any.Type!
  var leaf: Any.Type!
  var genericEnvironment: UnsafeRawPointer?
  let patternArgs: UnsafeRawPointer?

  init(patternArgs: UnsafeRawPointer?) {
    self.patternArgs = patternArgs
  }

  mutating func roundUpToPointerAlignment() {
    size = MemoryLayout<Int>._roundingUpToAlignment(size)
  }

  mutating func visitHeader(genericEnvironment: UnsafeRawPointer?,
                            rootMetadataRef: MetadataReference,
                            leafMetadataRef: MetadataReference,
                            kvcCompatibilityString: UnsafeRawPointer?) {
    self.genericEnvironment = genericEnvironment
    // Get the root and leaf type metadata so we can form the class type
    // for the entire key path.
    root = _resolveKeyPathMetadataReference(
              rootMetadataRef,
              genericEnvironment: genericEnvironment,
              arguments: patternArgs)
    leaf = _resolveKeyPathMetadataReference(
              leafMetadataRef,
              genericEnvironment: genericEnvironment,
              arguments: patternArgs)
  }

  mutating func visitStoredComponent(kind: KeyPathStructOrClass,
                                     mutable: Bool,
                                     offset: KeyPathPatternStoredOffset) {
    // Mutable class properties can be the root of a reference mutation.
    // Mutable struct properties pass through the existing capability.
    if mutable {
      switch kind {
      case .class:
        capability = .reference
      case .struct:
        break
      }
    } else {
      // Immutable properties can only be read.
      capability = .readOnly
    }

    // The size of the instantiated component depends on whether we can fit
    // the offset inline.
    switch offset {
    case .inline:
      size += 4

    case .outOfLine, .unresolvedFieldOffset, .unresolvedIndirectOffset:
      size += 8
    }
  }

  mutating func visitComputedComponent(mutating: Bool,
                                   idKind: KeyPathComputedIDKind,
                                   idResolution: KeyPathComputedIDResolution,
                                   idValueBase: UnsafeRawPointer,
                                   idValue: Int32,
                                   getter: UnsafeRawPointer,
                                   setter: UnsafeRawPointer?,
                                   arguments: KeyPathPatternComputedArguments?,
                                   externalArgs: UnsafeBufferPointer<Int32>?) {
    let settable = setter != nil

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
      _internalInvariantFailure("unpossible")
    }

    // Save space for the header...
    size += 4
    roundUpToPointerAlignment()
    // ...id, getter, and maybe setter...
    size += MemoryLayout<Int>.size * 2
    if settable {
      size += MemoryLayout<Int>.size
    }
    
    // ...and the arguments, if any.
    let argumentHeaderSize = MemoryLayout<Int>.size * 2
    switch (arguments, externalArgs) {
    case (nil, nil):
      break
    case (let arguments?, nil):
      size += argumentHeaderSize
      // If we have arguments, calculate how much space they need by invoking
      // the layout function.
      let (addedSize, addedAlignmentMask) = arguments.getLayout(patternArgs)
      // TODO: Handle over-aligned values
      _internalInvariant(addedAlignmentMask < MemoryLayout<Int>.alignment,
                   "overaligned computed property element not supported")
      size += addedSize
    
    case (let arguments?, let externalArgs?):
      // If we're referencing an external declaration, and it takes captured
      // arguments, then we have to build a bit of a chimera. The canonical
      // identity and accessors come from the descriptor, but the argument
      // handling is still as described in the local candidate.
      size += argumentHeaderSize
      let (addedSize, addedAlignmentMask) = arguments.getLayout(patternArgs)
      // TODO: Handle over-aligned values
      _internalInvariant(addedAlignmentMask < MemoryLayout<Int>.alignment,
                   "overaligned computed property element not supported")
      size += addedSize
      // We also need to store the size of the local arguments so we can
      // find the external component arguments.
      roundUpToPointerAlignment()
      size += RawKeyPathComponent.Header.externalWithArgumentsExtraSize
      size += MemoryLayout<Int>.size * externalArgs.count

    case (nil, let externalArgs?):
      // If we're instantiating an external property with a local
      // candidate that has no arguments, then things are a little
      // easier. We only need to instantiate the generic
      // arguments for the external component's accessors.
      size += argumentHeaderSize
      size += MemoryLayout<Int>.size * externalArgs.count
    }
  }

  mutating func visitOptionalChainComponent() {
    // Optional chaining forces the entire keypath to be read-only, even if
    // there are further reference-writable components.
    didChain = true
    capability = .readOnly
    size += 4
  }
  mutating func visitOptionalWrapComponent() {
    // Optional chaining forces the entire keypath to be read-only, even if
    // there are further reference-writable components.
    didChain = true
    capability = .readOnly
    size += 4
  }

  mutating func visitOptionalForceComponent() {
    // Force-unwrapping passes through the mutability of the preceding keypath.
    size += 4
  }

  mutating
  func visitIntermediateComponentType(metadataRef _: MetadataReference) {
    // The instantiated component type will be stored in the instantiated
    // object.
    roundUpToPointerAlignment()
    size += MemoryLayout<Int>.size
  }

  mutating func finish() {
  }
}

internal func _getKeyPathClassAndInstanceSizeFromPattern(
  _ pattern: UnsafeRawPointer,
  _ arguments: UnsafeRawPointer
) -> (
  keyPathClass: AnyKeyPath.Type,
  rootType: Any.Type,
  size: Int,
  alignmentMask: Int
) {
  var walker = GetKeyPathClassAndInstanceSizeFromPattern(patternArgs: arguments)
  _walkKeyPathPattern(pattern, walker: &walker)

  // Chaining always renders the whole key path read-only.
  if walker.didChain {
    walker.capability = .readOnly
  }

  // Grab the class object for the key path type we'll end up with.
  func openRoot<Root>(_: Root.Type) -> AnyKeyPath.Type {
    func openLeaf<Leaf>(_: Leaf.Type) -> AnyKeyPath.Type {
      switch walker.capability {
      case .readOnly:
        return KeyPath<Root, Leaf>.self
      case .value:
        return WritableKeyPath<Root, Leaf>.self
      case .reference:
        return ReferenceWritableKeyPath<Root, Leaf>.self
      }
    }
    return _openExistential(walker.leaf!, do: openLeaf)
  }
  let classTy = _openExistential(walker.root!, do: openRoot)

  return (keyPathClass: classTy,
          rootType: walker.root!,
          size: walker.size,
          // FIXME: Handle overalignment
          alignmentMask: MemoryLayout<Int>._alignmentMask)
}

internal struct InstantiateKeyPathBuffer : KeyPathPatternVisitor {
  var destData: UnsafeMutableRawBufferPointer
  var genericEnvironment: UnsafeRawPointer?
  let patternArgs: UnsafeRawPointer?
  var base: Any.Type

  init(destData: UnsafeMutableRawBufferPointer,
       patternArgs: UnsafeRawPointer?,
       root: Any.Type) {
    self.destData = destData
    self.patternArgs = patternArgs
    self.base = root
  }

  // Track the triviality of the resulting object data.
  var isTrivial: Bool = true

  // Track where the reference prefix begins.
  var endOfReferencePrefixComponent: UnsafeMutableRawPointer? = nil
  var previousComponentAddr: UnsafeMutableRawPointer? = nil

  mutating func pushDest<T>(_ value: T) {
    _internalInvariant(_isPOD(T.self))
    let size = MemoryLayout<T>.size
    let alignment = MemoryLayout<T>.alignment
    var baseAddress = destData.baseAddress.unsafelyUnwrapped
    var misalign = Int(bitPattern: baseAddress) % alignment
    if misalign != 0 {
      misalign = alignment - misalign
      baseAddress = baseAddress.advanced(by: misalign)
    }
    withUnsafeBytes(of: value) {
      _memcpy(dest: baseAddress, src: $0.baseAddress.unsafelyUnwrapped,
              size: UInt(size))
    }
    destData = UnsafeMutableRawBufferPointer(
      start: baseAddress + size,
      count: destData.count - size - misalign)
  }

  mutating func updatePreviousComponentAddr() -> UnsafeMutableRawPointer? {
    let oldValue = previousComponentAddr
    previousComponentAddr = destData.baseAddress.unsafelyUnwrapped
    return oldValue
  }

  mutating func visitHeader(genericEnvironment: UnsafeRawPointer?,
                            rootMetadataRef: MetadataReference,
                            leafMetadataRef: MetadataReference,
                            kvcCompatibilityString: UnsafeRawPointer?) {
    self.genericEnvironment = genericEnvironment
  }

  mutating func visitStoredComponent(kind: KeyPathStructOrClass,
                                     mutable: Bool,
                                     offset: KeyPathPatternStoredOffset) {
    let previous = updatePreviousComponentAddr()
    switch kind {
    case .class:
      // A mutable class property can end the reference prefix.
      if mutable {
        endOfReferencePrefixComponent = previous
      }
      fallthrough

    case .struct:
      // Resolve the offset.
      switch offset {
      case .inline(let value):
        let header = RawKeyPathComponent.Header(stored: kind,
                                                mutable: mutable,
                                                inlineOffset: value)
        pushDest(header)
      case .outOfLine(let offset):
        let header = RawKeyPathComponent.Header(storedWithOutOfLineOffset: kind,
                                                mutable: mutable)
        pushDest(header)
        pushDest(offset)
      case .unresolvedFieldOffset(let offsetOfOffset):
        // Look up offset in the type metadata. The value in the pattern is
        // the offset within the metadata object.
        let metadataPtr = unsafeBitCast(base, to: UnsafeRawPointer.self)
        let offset: UInt32
        switch kind {
        case .class:
          offset = UInt32(metadataPtr.load(fromByteOffset: Int(offsetOfOffset),
                                           as: UInt.self))
        case .struct:
          offset = UInt32(metadataPtr.load(fromByteOffset: Int(offsetOfOffset),
                                           as: UInt32.self))
        }

        let header = RawKeyPathComponent.Header(storedWithOutOfLineOffset: kind,
                                                mutable: mutable)
        pushDest(header)
        pushDest(offset)
      case .unresolvedIndirectOffset(let pointerToOffset):
        // Look up offset in the indirectly-referenced variable we have a
        // pointer.
        let offset = UInt32(pointerToOffset.pointee)
        let header = RawKeyPathComponent.Header(storedWithOutOfLineOffset: kind,
                                                mutable: mutable)
        pushDest(header)
        pushDest(offset)
      }
    }
  }

  mutating func visitComputedComponent(mutating: Bool,
                                   idKind: KeyPathComputedIDKind,
                                   idResolution: KeyPathComputedIDResolution,
                                   idValueBase: UnsafeRawPointer,
                                   idValue: Int32,
                                   getter: UnsafeRawPointer,
                                   setter: UnsafeRawPointer?,
                                   arguments: KeyPathPatternComputedArguments?,
                                   externalArgs: UnsafeBufferPointer<Int32>?) {
    let previous = updatePreviousComponentAddr()
    let settable = setter != nil
    // A nonmutating settable property can end the reference prefix.
    if settable && !mutating {
      endOfReferencePrefixComponent = previous
    }

    // Resolve the ID.
    let resolvedID: UnsafeRawPointer?

    switch idKind {
    case .storedPropertyIndex, .vtableOffset:
      _internalInvariant(idResolution == .resolved)
      // Zero-extend the integer value to get the instantiated id.
      let value = UInt(UInt32(bitPattern: idValue))
      resolvedID = UnsafeRawPointer(bitPattern: value)

    case .pointer:
      // Resolve the sign-extended relative reference.
      var absoluteID: UnsafeRawPointer? = idValueBase + Int(idValue)

      // If the pointer ID is unresolved, then it needs work to get to
      // the final value.
      switch idResolution {
      case .resolved:
        break

      case .indirectPointer:
        // The pointer in the pattern is an indirect pointer to the real
        // identifier pointer.
        absoluteID = absoluteID.unsafelyUnwrapped
          .load(as: UnsafeRawPointer?.self)

      case .functionCall:
        // The pointer in the pattern is to a function that generates the
        // identifier pointer.
        typealias Resolver = @convention(c) (UnsafeRawPointer?) -> UnsafeRawPointer?
        let resolverFn = unsafeBitCast(absoluteID.unsafelyUnwrapped,
                                       to: Resolver.self)

        absoluteID = resolverFn(patternArgs)
      }
      resolvedID = absoluteID
    }

    // Bring over the header, getter, and setter.
    let header = RawKeyPathComponent.Header(computedWithIDKind: idKind,
          mutating: mutating,
          settable: settable,
          hasArguments: arguments != nil || externalArgs != nil,
          instantiatedFromExternalWithArguments:
            arguments != nil && externalArgs != nil)
    pushDest(header)
    pushDest(resolvedID)
    pushDest(getter)
    if let setter = setter {
      pushDest(setter)
    }

    if let arguments = arguments {
      // Instantiate the arguments.
      let (baseSize, alignmentMask) = arguments.getLayout(patternArgs)
      _internalInvariant(alignmentMask < MemoryLayout<Int>.alignment,
                   "overaligned computed arguments not implemented yet")

      // The real buffer stride will be rounded up to alignment.
      var totalSize = (baseSize + alignmentMask) & ~alignmentMask

      // If an external property descriptor also has arguments, they'll be
      // added to the end with pointer alignment.
      if let externalArgs = externalArgs {
        totalSize = MemoryLayout<Int>._roundingUpToAlignment(totalSize)
        totalSize += MemoryLayout<Int>.size * externalArgs.count
      }

      pushDest(totalSize)
      pushDest(arguments.witnesses)

      // A nonnull destructor in the witnesses file indicates the instantiated
      // payload is nontrivial.
      if let _ = arguments.witnesses.pointee.destroy {
        isTrivial = false
      }

      // If the descriptor has arguments, store the size of its specific
      // arguments here, so we can drop them when trying to invoke
      // the component's witnesses.
      if let externalArgs = externalArgs {
        pushDest(externalArgs.count * MemoryLayout<Int>.size)
      }

      // Initialize the local candidate arguments here.
      _internalInvariant(Int(bitPattern: destData.baseAddress) & alignmentMask == 0,
                   "argument destination not aligned")
      arguments.initializer(patternArgs,
                            destData.baseAddress.unsafelyUnwrapped)

      destData = UnsafeMutableRawBufferPointer(
        start: destData.baseAddress.unsafelyUnwrapped + baseSize,
        count: destData.count - baseSize)
    }
    
    if let externalArgs = externalArgs {
      if arguments == nil {
        // If we're instantiating an external property without any local
        // arguments, then we only need to instantiate the arguments to the
        // property descriptor.
        let stride = MemoryLayout<Int>.size * externalArgs.count
        pushDest(stride)
        pushDest(__swift_keyPathGenericWitnessTable_addr())
      }

      // Write the descriptor's generic arguments, which should all be relative
      // references to metadata accessor functions.
      for i in externalArgs.indices {
        let base = externalArgs.baseAddress.unsafelyUnwrapped + i
        let offset = base.pointee
        let metadataRef = UnsafeRawPointer(base) + Int(offset)
        let result = _resolveKeyPathGenericArgReference(
                       metadataRef,
                       genericEnvironment: genericEnvironment,
                       arguments: patternArgs)
        pushDest(result)
      }
    }
  }

  mutating func visitOptionalChainComponent() {
    let _ = updatePreviousComponentAddr()
    let header = RawKeyPathComponent.Header(optionalChain: ())
    pushDest(header)
  }
  mutating func visitOptionalWrapComponent() {
    let _ = updatePreviousComponentAddr()
    let header = RawKeyPathComponent.Header(optionalWrap: ())
    pushDest(header)
  }
  mutating func visitOptionalForceComponent() {
    let _ = updatePreviousComponentAddr()
    let header = RawKeyPathComponent.Header(optionalForce: ())
    pushDest(header)
  }

  mutating func visitIntermediateComponentType(metadataRef: MetadataReference) {
    // Get the metadata for the intermediate type.
    let metadata = _resolveKeyPathMetadataReference(
                     metadataRef,
                     genericEnvironment: genericEnvironment,
                     arguments: patternArgs)
    pushDest(metadata)
    base = metadata
  }
  
  mutating func finish() {
    // Should have filled the entire buffer by the time we reach the end of the
    // pattern.
    _internalInvariant(destData.isEmpty,
                 "should have filled entire destination buffer")
  }
}

#if INTERNAL_CHECKS_ENABLED
// In debug builds of the standard library, check that instantiation produces
// components whose sizes are consistent with the sizing visitor pass.
internal struct ValidatingInstantiateKeyPathBuffer: KeyPathPatternVisitor {
  var sizeVisitor: GetKeyPathClassAndInstanceSizeFromPattern
  var instantiateVisitor: InstantiateKeyPathBuffer
  let origDest: UnsafeMutableRawPointer

  init(sizeVisitor: GetKeyPathClassAndInstanceSizeFromPattern,
       instantiateVisitor: InstantiateKeyPathBuffer) {
    self.sizeVisitor = sizeVisitor
    self.instantiateVisitor = instantiateVisitor
    origDest = self.instantiateVisitor.destData.baseAddress.unsafelyUnwrapped
  }

  mutating func visitHeader(genericEnvironment: UnsafeRawPointer?,
                            rootMetadataRef: MetadataReference,
                            leafMetadataRef: MetadataReference,
                            kvcCompatibilityString: UnsafeRawPointer?) {
    sizeVisitor.visitHeader(genericEnvironment: genericEnvironment,
                            rootMetadataRef: rootMetadataRef,
                            leafMetadataRef: leafMetadataRef,
                            kvcCompatibilityString: kvcCompatibilityString)
    instantiateVisitor.visitHeader(genericEnvironment: genericEnvironment,
                                 rootMetadataRef: rootMetadataRef,
                                 leafMetadataRef: leafMetadataRef,
                                 kvcCompatibilityString: kvcCompatibilityString)
  }
  mutating func visitStoredComponent(kind: KeyPathStructOrClass,
                                     mutable: Bool,
                                     offset: KeyPathPatternStoredOffset) {
    sizeVisitor.visitStoredComponent(kind: kind, mutable: mutable,
                                     offset: offset)
    instantiateVisitor.visitStoredComponent(kind: kind, mutable: mutable,
                                            offset: offset)
    checkSizeConsistency()
  }
  mutating func visitComputedComponent(mutating: Bool,
                                   idKind: KeyPathComputedIDKind,
                                   idResolution: KeyPathComputedIDResolution,
                                   idValueBase: UnsafeRawPointer,
                                   idValue: Int32,
                                   getter: UnsafeRawPointer,
                                   setter: UnsafeRawPointer?,
                                   arguments: KeyPathPatternComputedArguments?,
                                   externalArgs: UnsafeBufferPointer<Int32>?) {
    sizeVisitor.visitComputedComponent(mutating: mutating,
                                       idKind: idKind,
                                       idResolution: idResolution,
                                       idValueBase: idValueBase,
                                       idValue: idValue,
                                       getter: getter,
                                       setter: setter,
                                       arguments: arguments,
                                       externalArgs: externalArgs)
    instantiateVisitor.visitComputedComponent(mutating: mutating,
                                       idKind: idKind,
                                       idResolution: idResolution,
                                       idValueBase: idValueBase,
                                       idValue: idValue,
                                       getter: getter,
                                       setter: setter,
                                       arguments: arguments,
                                       externalArgs: externalArgs)
    checkSizeConsistency()
  }
  mutating func visitOptionalChainComponent() {
    sizeVisitor.visitOptionalChainComponent()
    instantiateVisitor.visitOptionalChainComponent()
    checkSizeConsistency()
  }
  mutating func visitOptionalWrapComponent() {
    sizeVisitor.visitOptionalWrapComponent()
    instantiateVisitor.visitOptionalWrapComponent()
    checkSizeConsistency()
  }
  mutating func visitOptionalForceComponent() {
    sizeVisitor.visitOptionalForceComponent()
    instantiateVisitor.visitOptionalForceComponent()
    checkSizeConsistency()
  }
  mutating func visitIntermediateComponentType(metadataRef: MetadataReference) {
    sizeVisitor.visitIntermediateComponentType(metadataRef: metadataRef)
    instantiateVisitor.visitIntermediateComponentType(metadataRef: metadataRef)
    checkSizeConsistency()
  }

  mutating func finish() {
    sizeVisitor.finish()
    instantiateVisitor.finish()
    checkSizeConsistency()
  }

  func checkSizeConsistency() {
    let nextDest = instantiateVisitor.destData.baseAddress.unsafelyUnwrapped
    let curSize = nextDest - origDest + MemoryLayout<Int>.size

    _internalInvariant(curSize == sizeVisitor.size,
                 "size and instantiation visitors out of sync")
  }
}
#endif // INTERNAL_CHECKS_ENABLED

internal func _instantiateKeyPathBuffer(
  _ pattern: UnsafeRawPointer,
  _ origDestData: UnsafeMutableRawBufferPointer,
  _ rootType: Any.Type,
  _ arguments: UnsafeRawPointer
) {
  let destHeaderPtr = origDestData.baseAddress.unsafelyUnwrapped
  var destData = UnsafeMutableRawBufferPointer(
    start: destHeaderPtr.advanced(by: MemoryLayout<Int>.size),
    count: origDestData.count - MemoryLayout<Int>.size)

#if INTERNAL_CHECKS_ENABLED
  // If checks are enabled, use a validating walker that ensures that the
  // size pre-walk and instantiation walk are in sync.
  let sizeWalker = GetKeyPathClassAndInstanceSizeFromPattern(
    patternArgs: arguments)
  let instantiateWalker = InstantiateKeyPathBuffer(
    destData: destData,
    patternArgs: arguments,
    root: rootType)
  
  var walker = ValidatingInstantiateKeyPathBuffer(sizeVisitor: sizeWalker,
                                          instantiateVisitor: instantiateWalker)
#else
  var walker = InstantiateKeyPathBuffer(
    destData: destData,
    patternArgs: arguments,
    root: rootType)
#endif

  _walkKeyPathPattern(pattern, walker: &walker)

#if INTERNAL_CHECKS_ENABLED
  let isTrivial = walker.instantiateVisitor.isTrivial
  let endOfReferencePrefixComponent =
    walker.instantiateVisitor.endOfReferencePrefixComponent
#else
  let isTrivial = walker.isTrivial
  let endOfReferencePrefixComponent = walker.endOfReferencePrefixComponent
#endif

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

