// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -swift-version 5 -enable-library-evolution -emit-module-interface-path %t/Bakery.swiftinterface -DBAKERY -module-name Bakery
// RUN: %target-swift-frontend -typecheck %s -swift-version 5 -enable-library-evolution -emit-module-interface-path %t/Home.swiftinterface -DHOME -module-name Home -I %t
// RUN: %FileCheck %s < %t/Home.swiftinterface

#if BAKERY
public class Cake {
    public init(_: ()) {}
}
#endif

#if HOME
import Bakery

struct Cake {
    public init() {}
}

extension Bakery.Cake {
    public convenience init() {}
}
#endif

// CHECK: extension Bakery.Cake
