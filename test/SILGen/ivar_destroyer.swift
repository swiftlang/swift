// RUN: %target-swift-emit-silgen -parse-as-library -enable-sil-ownership %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @$S14ivar_destroyer36DerivedClassWithNonTrivialPropertiesCfE
// CHECK:       bb0(%0 : @guaranteed $DerivedClassWithNonTrivialProperties):
// CHECK-NEXT:    debug_value %0
// CHECK-NEXT:    [[Z_ADDR:%.*]] = ref_element_addr %0
// CHECK-NEXT:    destroy_addr [[Z_ADDR]]
// CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:    return [[RESULT]]

// CHECK-LABEL: sil_vtable RootClassWithoutProperties {
// CHECK-NEXT:    #RootClassWithoutProperties.init!initializer.1
// CHECK-NEXT:    #RootClassWithoutProperties.deinit!deallocator
// CHECK-NEXT:  }

// CHECK-LABEL: sil_vtable RootClassWithTrivialProperties {
// CHECK-NEXT:    #RootClassWithTrivialProperties.x!getter.1
// CHECK-NEXT:    #RootClassWithTrivialProperties.x!setter.1
// CHECK-NEXT:    #RootClassWithTrivialProperties.x!materializeForSet.1
// CHECK-NEXT:    #RootClassWithTrivialProperties.y!getter.1
// CHECK-NEXT:    #RootClassWithTrivialProperties.y!setter.1
// CHECK-NEXT:    #RootClassWithTrivialProperties.y!materializeForSet.1
// CHECK-NEXT:    #RootClassWithTrivialProperties.init!initializer.1
// CHECK-NEXT:    #RootClassWithTrivialProperties.deinit!deallocator
// CHECK-NEXT:  }

// CHECK-LABEL: sil_vtable RootClassWithNonTrivialProperties {
// CHECK-NEXT:    #RootClassWithNonTrivialProperties.x!getter.1
// CHECK-NEXT:    #RootClassWithNonTrivialProperties.x!setter.1
// CHECK-NEXT:    #RootClassWithNonTrivialProperties.x!materializeForSet.1
// CHECK-NEXT:    #RootClassWithNonTrivialProperties.init!initializer.1
// CHECK-NEXT:    #RootClassWithNonTrivialProperties.deinit!deallocator
// CHECK-NEXT:  }

// CHECK-LABEL: sil_vtable DerivedClassWithTrivialProperties {
// CHECK-NEXT:    #RootClassWithoutProperties.init!initializer.1
// CHECK-NEXT:    #DerivedClassWithTrivialProperties.z!getter.1
// CHECK-NEXT:    #DerivedClassWithTrivialProperties.z!setter.1
// CHECK-NEXT:    #DerivedClassWithTrivialProperties.z!materializeForSet.1
// CHECK-NEXT:    #DerivedClassWithTrivialProperties.deinit!deallocator
// CHECK-NEXT:  }

// CHECK-LABEL: sil_vtable DerivedClassWithNonTrivialProperties {
// CHECK-NEXT:    #RootClassWithoutProperties.init!initializer.1
// CHECK-NEXT:    #DerivedClassWithNonTrivialProperties.z!getter.1
// CHECK-NEXT:    #DerivedClassWithNonTrivialProperties.z!setter.1
// CHECK-NEXT:    #DerivedClassWithNonTrivialProperties.z!materializeForSet.1
// CHECK-NEXT:    #DerivedClassWithNonTrivialProperties.deinit!deallocator
// CHECK-NEXT:    #DerivedClassWithNonTrivialProperties!ivardestroyer.1
// CHECK-NEXT:  }
