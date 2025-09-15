// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-ir -primary-file %s -primary-file %S/Inputs/opaque_result_type_private_underlying_2.swift | %FileCheck %s --check-prefix=SINGLEMODULE
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-module -emit-module-path=%t/Repo1.swiftmodule -module-name=Repo1 %S/Inputs/opaque_result_type_private_underlying_2.swift
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -I %t -emit-ir -primary-file %s  -DUSEMODULE | %FileCheck %s --check-prefix=NONRESILIENT
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -enable-library-evolution -emit-module -emit-module-path=%t/Repo1.swiftmodule -module-name=Repo1 %S/Inputs/opaque_result_type_private_underlying_2.swift
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -I %t -emit-ir -primary-file %s  -DUSEMODULE | %FileCheck %s --check-prefix=RESILIENT
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -enable-library-evolution -emit-module -emit-module-path=%t/Repo1.swiftmodule -module-name=Repo1 %S/Inputs/opaque_result_type_private_underlying_2.swift
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -I %t -emit-ir -primary-file %s -primary-file %S/Inputs/opaque_result_type_private_underlying_3.swift -DUSEMODULE -DUSESECONDFILE | %FileCheck %s --check-prefix=RESILIENT

#if USEMODULE
import Repo1
#endif

// SINGLEMODULE: s37opaque_result_type_private_underlying18UsePublicInlinableVAA1AAAMA" = internal constant { {{.*}}symbolic {{(_____ )?}}37opaque_result_type_private_underlying7PublicSV
// NONRESILIENT: s37opaque_result_type_private_underlying18UsePublicInlinableV5Repo11AAAMA" = internal constant { {{.*}}symbolic {{(_____ )?}}5Repo17PublicSV
// RESILIENT: s37opaque_result_type_private_underlying18UsePublicInlinableV5Repo11AAAMA" = internal constant { {{.*}}symbolic {{(_____ )?}}5Repo17PublicSV
public struct UsePublicInlinable : A {
  public init() {}
  public func bindAssoc() -> some Q {
    return PublicUnderlyingInlinable().bindAssoc()
  }
}

// SINGLEMODULE: s37opaque_result_type_private_underlying9UsePublicVAA1AAAMA" = internal constant { {{.*}}symbolic {{(_____ )?}}37opaque_result_type_private_underlying7PublicSV
// NONRESILIENT: s37opaque_result_type_private_underlying9UsePublicV5Repo11AAAMA" = internal constant { {{.*}}symbolic {{(_____ )?}}5Repo17PublicSV
// RESILIENT: s37opaque_result_type_private_underlying9UsePublicV5Repo11AAAMA" = internal constant {  {{.*}}symbolic _____y_Qo_ 5Repo116PublicUnderlyingV9bindAssocQryFQO
public struct UsePublic : A {
  public init() {}
  public func bindAssoc() -> some Q {
    return PublicUnderlying().bindAssoc()
  }
}

// SINGLEMODULE: s37opaque_result_type_private_underlying11UseInternalVAA1AAAMA" = internal constant { {{.*}}symbolic {{(_____ )?}}37opaque_result_type_private_underlying9InternalSV
// NONRESILIENT: s37opaque_result_type_private_underlying11UseInternalV5Repo11AAAMA" = internal constant { {{.*}}symbolic _____y_Qo_ 5Repo118InternalUnderlyingV9bindAssocQryFQO
// RESILIENT: s37opaque_result_type_private_underlying11UseInternalV5Repo11AAAMA" = internal constant { {{.*}}symbolic _____y_Qo_ 5Repo118InternalUnderlyingV9bindAssocQryFQO
public struct UseInternal: A {
  public init() {}
  public func bindAssoc() -> some Q {
    return InternalUnderlying().bindAssoc()
  }
}

// SINGLEMODULE: s37opaque_result_type_private_underlying10UsePrivateVAA1AAAMA" = internal constant { {{.*}}symbolic _____y_Qo_ 37opaque_result_type_private_underlying17PrivateUnderlyingV9bindAssocQryFQO
// NONRESILIENT: s37opaque_result_type_private_underlying10UsePrivateV5Repo11AAAMA" = internal constant { {{.*}}symbolic _____y_Qo_ 5Repo117PrivateUnderlyingV9bindAssocQryFQO
// RESILIENT: s37opaque_result_type_private_underlying10UsePrivateV5Repo11AAAMA" = internal constant { {{.*}}symbolic _____y_Qo_ 5Repo117PrivateUnderlyingV9bindAssocQryFQO
public struct UsePrivate: A {
  public init() {}
  public func bindAssoc() -> some Q {
    return PrivateUnderlying().bindAssoc()
  }
}

#if USESECONDFILE
public struct MyThing {
    public let which: MyEnum
    public init(_ which: MyEnum) {
      self.which = which
    }

    public var body: some Q {
        self.thing
    }
    public var thing: some Q {
        self.which.thing
    }
}
#endif

public struct E<V> {
   var body : some P {
     var r = R(V.self, getSome())
     r.modify()
     return r
   }
}
