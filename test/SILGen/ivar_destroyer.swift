// RUN: %target-swift-emit-silgen -parse-as-library %s | %FileCheck %s

// Only derived classes with non-trivial ivars need an ivar destroyer.

struct TrivialStruct {}

class RootClassWithoutProperties {}

class RootClassWithTrivialProperties {
  var x: Int = 0
  var y: TrivialStruct = TrivialStruct()
}

class Canary {}

class RootClassWithNonTrivialProperties {
  var x: Canary = Canary()
}

class DerivedClassWithTrivialProperties : RootClassWithoutProperties {
  var z: Int = 12
}

class DerivedClassWithNonTrivialProperties : RootClassWithoutProperties {
  var z: Canary = Canary()
}

// CHECK-LABEL: sil hidden [ossa] @$s14ivar_destroyer36DerivedClassWithNonTrivialPropertiesCfE
// CHECK:       bb0(%0 : @guaranteed $DerivedClassWithNonTrivialProperties):
// CHECK-NEXT:    debug_value %0
// CHECK-NEXT:    [[Z_ADDR:%.*]] = ref_element_addr %0
// CHECK-NEXT:    [[Z_ADDR_DEINIT_ACCESS:%.*]] = begin_access [deinit] [static] [[Z_ADDR]]
// CHECK-NEXT:    destroy_addr [[Z_ADDR_DEINIT_ACCESS]]
// CHECK-NEXT:    end_access [[Z_ADDR_DEINIT_ACCESS]]
// CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:    return [[RESULT]]

// CHECK-LABEL: sil_vtable RootClassWithoutProperties {
// CHECK-NEXT:    #RootClassWithoutProperties.init!allocator
// CHECK-NEXT:    #RootClassWithoutProperties.deinit!deallocator
// CHECK-NEXT:  }

// CHECK-LABEL: sil_vtable RootClassWithTrivialProperties {
// CHECK-NEXT:    #RootClassWithTrivialProperties.x!getter
// CHECK-NEXT:    #RootClassWithTrivialProperties.x!setter
// CHECK-NEXT:    #RootClassWithTrivialProperties.x!modify
// CHECK-NEXT:    #RootClassWithTrivialProperties.y!getter
// CHECK-NEXT:    #RootClassWithTrivialProperties.y!setter
// CHECK-NEXT:    #RootClassWithTrivialProperties.y!modify
// CHECK-NEXT:    #RootClassWithTrivialProperties.init!allocator
// CHECK-NEXT:    #RootClassWithTrivialProperties.deinit!deallocator
// CHECK-NEXT:  }

// CHECK-LABEL: sil_vtable RootClassWithNonTrivialProperties {
// CHECK-NEXT:    #RootClassWithNonTrivialProperties.x!getter
// CHECK-NEXT:    #RootClassWithNonTrivialProperties.x!setter
// CHECK-NEXT:    #RootClassWithNonTrivialProperties.x!modify
// CHECK-NEXT:    #RootClassWithNonTrivialProperties.init!allocator
// CHECK-NEXT:    #RootClassWithNonTrivialProperties.deinit!deallocator
// CHECK-NEXT:  }

// CHECK-LABEL: sil_vtable DerivedClassWithTrivialProperties {
// CHECK-NEXT:    #RootClassWithoutProperties.init!allocator
// CHECK-NEXT:    #DerivedClassWithTrivialProperties.z!getter
// CHECK-NEXT:    #DerivedClassWithTrivialProperties.z!setter
// CHECK-NEXT:    #DerivedClassWithTrivialProperties.z!modify
// CHECK-NEXT:    #DerivedClassWithTrivialProperties.deinit!deallocator
// CHECK-NEXT:  }

// CHECK-LABEL: sil_vtable DerivedClassWithNonTrivialProperties {
// CHECK-NEXT:    #RootClassWithoutProperties.init!allocator
// CHECK-NEXT:    #DerivedClassWithNonTrivialProperties.z!getter
// CHECK-NEXT:    #DerivedClassWithNonTrivialProperties.z!setter
// CHECK-NEXT:    #DerivedClassWithNonTrivialProperties.z!modify
// CHECK-NEXT:    #DerivedClassWithNonTrivialProperties.deinit!deallocator
// CHECK-NEXT:    #DerivedClassWithNonTrivialProperties!ivardestroyer
// CHECK-NEXT:  }
