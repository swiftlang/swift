// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-objc-interop -disable-objc-attr-requires-foundation-module -import-objc-header %S/Inputs/convenience_init_peer_delegation_import.h %s | %FileCheck %s

extension ImportedClass {
  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE17swiftToDesignatedAByt_tcfC
  // CHECK: objc_method {{%.+}} : $ImportedClass, #ImportedClass.init!initializer.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE17swiftToDesignatedAByt_tcfC'
  convenience init(swiftToDesignated: ()) {
    self.init()
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE18swiftToConvenienceAByt_tcfC
  // CHECK: objc_method {{%.+}} : $ImportedClass, #ImportedClass.init!initializer.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE18swiftToConvenienceAByt_tcfC'
  convenience init(swiftToConvenience: ()) {
    self.init(conveniently: ())
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE25swiftToConvenienceFactoryAByt_tcfC
  // CHECK: objc_method {{%.+}} : $@objc_metatype ImportedClass.Type, #ImportedClass.init!allocator.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE25swiftToConvenienceFactoryAByt_tcfC'
  convenience init(swiftToConvenienceFactory: ()) {
    self.init(convenientFactory: false)
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE20swiftToNormalFactoryAByt_tcfC
  // CHECK: objc_method {{%.+}} : $@objc_metatype ImportedClass.Type, #ImportedClass.init!allocator.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE20swiftToNormalFactoryAByt_tcfC'
  convenience init(swiftToNormalFactory: ()) {
    // FIXME: This shouldn't be allowed, since the factory won't actually use
    // the dynamic Self type.
    self.init(normalFactory: false)
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE16objcToDesignatedAByt_tcfC
  // CHECK: function_ref @$sSo13ImportedClassC39convenience_init_peer_delegation_importE16objcToDesignatedAByt_tcfcTD :
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE16objcToDesignatedAByt_tcfC'
  // CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE16objcToDesignatedAByt_tcfcTD
  // CHECK: objc_method %0 : $ImportedClass, #ImportedClass.init!initializer.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE16objcToDesignatedAByt_tcfcTD'
  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE16objcToDesignatedAByt_tcfc
  // CHECK: objc_method {{%.+}} : $ImportedClass, #ImportedClass.init!initializer.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE16objcToDesignatedAByt_tcfc'
  // CHECK-LABEL: sil private [thunk] [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE16objcToDesignatedAByt_tcfcTo
  // CHECK: function_ref @$sSo13ImportedClassC39convenience_init_peer_delegation_importE16objcToDesignatedAByt_tcfc :
  @objc convenience init(objcToDesignated: ()) {
    self.init()
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE17objcToConvenienceAByt_tcfC
  // CHECK: function_ref @$sSo13ImportedClassC39convenience_init_peer_delegation_importE17objcToConvenienceAByt_tcfcTD :
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE17objcToConvenienceAByt_tcfC'
  // CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE17objcToConvenienceAByt_tcfcTD
  // CHECK: objc_method %0 : $ImportedClass, #ImportedClass.init!initializer.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE17objcToConvenienceAByt_tcfcTD'
  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE17objcToConvenienceAByt_tcfc
  // CHECK: objc_method {{%.+}} : $ImportedClass, #ImportedClass.init!initializer.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE17objcToConvenienceAByt_tcfc'
  // CHECK-LABEL: sil private [thunk] [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE17objcToConvenienceAByt_tcfcTo
  // CHECK: function_ref @$sSo13ImportedClassC39convenience_init_peer_delegation_importE17objcToConvenienceAByt_tcfc :
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE17objcToConvenienceAByt_tcfcTo'
  @objc convenience init(objcToConvenience: ()) {
    self.init(conveniently: ())
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE24objcToConvenienceFactoryAByt_tcfC
  // CHECK: function_ref @$sSo13ImportedClassC39convenience_init_peer_delegation_importE24objcToConvenienceFactoryAByt_tcfcTD :
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE24objcToConvenienceFactoryAByt_tcfC'
  // CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE24objcToConvenienceFactoryAByt_tcfcTD
  // CHECK: objc_method %0 : $ImportedClass, #ImportedClass.init!initializer.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE24objcToConvenienceFactoryAByt_tcfcTD'
  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE24objcToConvenienceFactoryAByt_tcfc
  // CHECK: objc_method {{%.+}} : $@objc_metatype ImportedClass.Type, #ImportedClass.init!allocator.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE24objcToConvenienceFactoryAByt_tcfc'
  // CHECK-LABEL: sil private [thunk] [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE24objcToConvenienceFactoryAByt_tcfcTo
  // CHECK: function_ref @$sSo13ImportedClassC39convenience_init_peer_delegation_importE24objcToConvenienceFactoryAByt_tcfc :
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE24objcToConvenienceFactoryAByt_tcfcTo'
  @objc convenience init(objcToConvenienceFactory: ()) {
    self.init(convenientFactory: false)
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE19objcToNormalFactoryAByt_tcfC
  // CHECK: function_ref @$sSo13ImportedClassC39convenience_init_peer_delegation_importE19objcToNormalFactoryAByt_tcfcTD :
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE19objcToNormalFactoryAByt_tcfC'
  // CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE19objcToNormalFactoryAByt_tcfcTD
  // CHECK: objc_method %0 : $ImportedClass, #ImportedClass.init!initializer.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE19objcToNormalFactoryAByt_tcfcTD'
  // CHECK-LABEL: sil hidden [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE19objcToNormalFactoryAByt_tcfc
  // CHECK: objc_method {{%.+}} : $@objc_metatype ImportedClass.Type, #ImportedClass.init!allocator.foreign
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE19objcToNormalFactoryAByt_tcfc'
  // CHECK-LABEL: sil private [thunk] [ossa] @$sSo13ImportedClassC39convenience_init_peer_delegation_importE19objcToNormalFactoryAByt_tcfcTo
  // CHECK: function_ref @$sSo13ImportedClassC39convenience_init_peer_delegation_importE19objcToNormalFactoryAByt_tcfc :
  // CHECK: end sil function '$sSo13ImportedClassC39convenience_init_peer_delegation_importE19objcToNormalFactoryAByt_tcfcTo'
  @objc convenience init(objcToNormalFactory: ()) {
    // FIXME: This shouldn't be allowed, since the factory won't actually use
    // the dynamic Self type.
    self.init(normalFactory: false)
  }
}
