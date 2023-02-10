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

@available(SwiftStdlib 5.9, *)
struct TypeCache {
  var cache: Lock<[Key: Any.Type?]>
  
  @available(SwiftStdlib 5.9, *)
  init() {
    cache = Lock<[Key: Any.Type?]>(initialValue: [:])
  }
}

@available(SwiftStdlib 5.9, *)
extension TypeCache {
  struct Key {
    let typeRef: MangledTypeReference
    let typeMetadata: TypeMetadata
    
    init(_ typeRef: MangledTypeReference, _ typeMetadata: TypeMetadata) {
      self.typeRef = typeRef
      self.typeMetadata = typeMetadata
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension TypeCache.Key: Equatable {
  static func ==(_ lhs: TypeCache.Key, _ rhs: TypeCache.Key) -> Bool {
    var lhsEnd = lhs.typeRef.ptr
    var rhsEnd = rhs.typeRef.ptr
    
    var isGeneric = false
    
  loop: while let lhsCurrent = Optional(lhsEnd.unprotectedLoad(as: UInt8.self)),
              let rhsCurrent = Optional(rhsEnd.unprotectedLoad(as: UInt8.self)) {
      lhsEnd += 1
      rhsEnd += 1
      
      guard lhsCurrent == rhsCurrent else {
        return false
      }
      
      switch lhsCurrent {
      case 0x0:
        break loop
      
      // Direct reference to context descriptor
      case 0x1:
        let lhsAddr = lhsEnd.relativeDirectAddress(as: ContextDescriptor.self)
        let rhsAddr = rhsEnd.relativeDirectAddress(as: ContextDescriptor.self)
        
        guard lhsAddr == rhsAddr else {
          return false
        }
        
        lhsEnd += MemoryLayout<RelativeDirectPointer<ContextDescriptor>>.size
        rhsEnd += MemoryLayout<RelativeDirectPointer<ContextDescriptor>>.size
        
      // Indirect reference to context descriptor
      case 0x2:
        let lhsAddr = lhsEnd.relativeIndirectAddress(as: ContextDescriptor.self)
        let rhsAddr = rhsEnd.relativeIndirectAddress(as: ContextDescriptor.self)
        
        guard lhsAddr == rhsAddr else {
          return false
        }
        
        lhsEnd += MemoryLayout<RelativeIndirectPointer<ContextDescriptor>>.size
        rhsEnd += MemoryLayout<RelativeIndirectPointer<ContextDescriptor>>.size
        
      // Note: This is somewhat of a hack to get a rough estimate of whether
      // this type ref is generic (the resulting type is dependent on the
      // generic context passed in). Abuse the fact that type refs want to
      // eagerly use symbolic references and don't use them in very niche
      // cases. In those cases, mistaking them for generic type refs is fine,
      // we'll just be dependent on the metadata for the final hash in the
      // type cache. 'q' as a standard substitution is 'Swift.Optional', but
      // optional fields are mangled with the shorter version of 'Sg' instead
      // of 'ySqG'. 'x' as a standard substitution is 'Swift.Strideable'.
      //
      // 'q' and 'x' respectfully.
      case 0x71, 0x78:
        isGeneric = true
        
      default:
        continue
      }
    }
    
    guard isGeneric else {
      return true
    }
    
    return lhs.typeMetadata == rhs.typeMetadata
  }
}

@available(SwiftStdlib 5.9, *)
extension TypeCache.Key: Hashable {
  func hash(into hasher: inout Hasher) {
    var isGeneric = false
    
    var end = typeRef.ptr
    
    while let current = Optional(end.unprotectedLoad(as: UInt8.self)), current != 0 {
      end += 1
      
      switch current {
        // Direct reference to context descriptor
      case 0x1:
        // Hash the stable address
        hasher.combine(end.relativeDirectAddress(as: ContextDescriptor.self))
        
        end += MemoryLayout<RelativeDirectPointer<ContextDescriptor>>.size
        
      case 0x2:
        // Hash the stable address
        hasher.combine(end.relativeIndirectAddress(as: ContextDescriptor.self))
        
        end += MemoryLayout<RelativeIndirectPointer<ContextDescriptor>>.size
        
      // Note: This is somewhat of a hack to get a rough estimate of whether
      // this type ref is generic (the resulting type is dependent on the
      // generic context passed in). Abuse the fact that type refs want to
      // eagerly use symbolic references and don't use them in very niche
      // cases. In those cases, mistaking them for generic type refs is fine,
      // we'll just be dependent on the metadata for the final hash in the
      // type cache. 'q' as a standard substitution is 'Swift.Optional', but
      // optional fields are mangled with the shorter version of 'Sg' instead
      // of 'ySqG'. 'x' as a standard substitution is 'Swift.Strideable'.
      //
      // 'q' and 'x' respectfully.
      case 0x71, 0x78:
        isGeneric = true
        fallthrough
        
      default:
        hasher.combine(current)
      }
    }
    
    hasher.combine(0 as UInt8)
    
    guard isGeneric else {
      return
    }
    
    hasher.combine(typeMetadata)
  }
}

@available(SwiftStdlib 5.9, *)
extension TypeCache {
  func getOrInsert(
    _ typeRef: MangledTypeReference,
    from typeMetadata: TypeMetadata
  ) -> Any.Type? {
    let key = Key(typeRef, typeMetadata)
    
    return cache.withLock {
      if let type = $0[key] {
        return type
      }
      
      let length = getSymbolicMangledNameLength(typeRef.ptr)
      let type = _getTypeByMangledNameInContext(
        UnsafePointer(typeRef.ptr._rawValue),
        UInt(truncatingIfNeeded: length),
        genericContext: typeMetadata.descriptor.ptr,
        genericArguments: typeMetadata.genericArguments
      )
      
      $0[key] = type
      return type
    }
  }
}

@available(SwiftStdlib 5.9, *)
var typeCache: TypeCache = {
  var result = TypeCache()

  result.cache.withLock {
    $0.reserveCapacity(25)
  }

  return result
}()
