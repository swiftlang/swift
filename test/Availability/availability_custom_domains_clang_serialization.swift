// REQUIRES: swift_feature_CustomAvailability
// RUN: %empty-directory(%t/mod)
// RUN: %target-swift-frontend -emit-module %s -o %t/mod/Foo.swiftmodule -parse-as-library -enable-library-evolution -module-name Foo -import-bridging-header %S/Inputs/AvailabilityDomains.h -enable-experimental-feature CustomAvailability -D FOO
// RUN: %target-swift-frontend -emit-module %s -o %t/mod/Bar.swiftmodule -parse-as-library -enable-library-evolution -module-name Bar -import-bridging-header %S/Inputs/AvailabilityDomains.h -enable-experimental-feature CustomAvailability -I %t/mod -D BAR

#if FOO

@available(DynamicDomain)
public struct X {
    public init() { }
}

public struct Z {
    public init() {
        if #available(DynamicDomain) {
            print("#available")
            print(X())
        } else {
            print("#unavailable")
        }
    }
}

#endif

#if BAR

import Foo

public struct Y {
    init() {
        if #available(DynamicDomain) {
            print("#available")
            print(X())
            print(Z())
        } else {
            print("#unavailable")
            print(Z())
        }
    }

}
#endif