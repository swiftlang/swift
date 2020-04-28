import SwiftRemoteMirror

extension SwiftReflectionContextRef {
  func name(metadata: swift_reflection_ptr_t) -> String? {
    let tr = swift_reflection_typeRefForMetadata(self, UInt(metadata));
    guard tr != 0 else { return nil }

    guard let cstr = swift_reflection_copyDemangledNameForTypeRef(self, tr)
      else { return nil }
    defer { free(cstr) }
    return String(cString: cstr)
  }

  func name(proto: swift_reflection_ptr_t) -> String? {
    guard let cstr = swift_reflection_copyDemangledNameForProtocolDescriptor(
      self, proto) else { return nil }
    defer { free(cstr) }
    return String(cString: cstr)
  }

  func iterateConformanceCache(
    call: (swift_reflection_ptr_t, swift_reflection_ptr_t) -> Void) -> Bool {
    var call = call
    let success = swift_reflection_iterateConformanceCache(self, {
      let callPtr = $2!.bindMemory(to: ((swift_reflection_ptr_t, swift_reflection_ptr_t) -> Void).self, capacity: 1)
      callPtr.pointee($0, $1)
    }, &call)
    return success != 0
  }

  func iterateMetadataAllocations(call: (swift_metadata_allocation_t) -> Void)
    -> Bool {
    var call = call
    let success = swift_reflection_iterateMetadataAllocations(self, {
      let callPtr = $1!.bindMemory(to:
        ((swift_metadata_allocation_t) -> Void).self, capacity: 1)
      callPtr.pointee($0)
    }, &call)
    return success != 0
  }

  func metadataPointer(allocation: swift_metadata_allocation_t)
    -> swift_reflection_ptr_t {
    return swift_reflection_allocationMetadataPointer(self, allocation)
  }
}
