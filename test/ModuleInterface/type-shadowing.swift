// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Bakery.swiftinterface) %s -DBAKERY -module-name Bakery
// RUN: %target-swift-typecheck-module-from-interface(%t/Bakery.swiftinterface) -module-name Bakery
// RUN: %target-swift-emit-module-interface(%t/Home.swiftinterface) %s -DHOME -module-name Home -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Home.swiftinterface) -module-name Home -I %t
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
