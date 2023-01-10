//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift
import _Runtime

extension KeyPath {
  @usableFromInline
  static func create(for field: Field) -> KeyPath {
    let result = Builtin.allocWithTailElems_1(
      self,
      6._builtinWordValue,
      Int32.self
    )
    
    var base = unsafeBitCast(result, to: UnsafeMutableRawPointer.self)
    base += MemoryLayout<Int>.size * 2
    
    // The first word is the KVC string pointer. Set it to null.
    base.storeBytes(of: 0, as: Int.self)
    base += MemoryLayout<Int>.size
    
    base.storeBytes(of: 4 | 0x80000000, as: UInt32.self)
    // Components start on word alignment
    base += MemoryLayout<Int>.size
    
    var component = UInt32(truncatingIfNeeded: field.offset)
    
    if field.parent.kind == .struct {
      component |= 1 << 24
    } else if field.parent.kind == .class {
      component |= 3 << 24
    } else {
      assert(field.parent.kind == .tuple)
      component |= 1 << 24
    }
    
    base.storeBytes(of: component, as: UInt32.self)
    
    return result
  }
}

extension Case {
  @inlinable
  public func get(from instance: Any) -> Any? {
    guard Type(instance).metadata.ptr == parent.ptr else {
      return nil
    }
    
    var instance = unsafeBitCast(instance, to: AnyExistentialContainer.self)
    if instance.projectValue({
      guard parent.enumVWT.getEnumTag($0) == tag else {
        return true
      }
      
      return false
    }) {
      return nil
    }
    
    var copy = AnyExistentialContainer(metadata: Metadata(parent.ptr))
    
    instance.projectValue { src in
      copy.allocateBox { dest in
        parent.vwt.initializeWithCopy(dest, src)
      }
    }
    
    let boxTy: Metadata
    
    if isIndirect {
      boxTy = Metadata(Builtin.NativeObject.self)
    } else {
      boxTy = payloadType!.metadata
    }
    
    let pair = swift_allocBox(boxTy)
    
    copy.projectValue {
      parent.enumVWT.destructiveProjectEnumData($0)
      
      boxTy.vwt.initializeWithTake(pair.buffer, $0)
    }
    
    var value = pair.buffer
    
    if isIndirect {
      let owner = value.loadUnaligned(as: HeapObject.self)
      value = swift_projectBox(owner)
    }
    
    var result = AnyExistentialContainer(metadata: payloadType!.metadata)
    
    result.allocateBox {
      parent.vwt.initializeWithCopy($0, value)
    }
    
    return unsafeBitCast(result, to: Any.self)
  }
}
