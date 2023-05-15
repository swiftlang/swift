// RUN: %empty-directory(%t)
// RUN: split-file %s %t

//--- SPIDependency.swift

// RUN: %target-swift-frontend -emit-module %t/SPIDependency.swift -o %t \
// RUN:   -enable-library-evolution -swift-version 5

//--- Lib.swift

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -o %t -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Lib.private.swiftinterface \
// RUN:   -experimental-spi-only-imports

@_spiOnly import SPIDependency

//--- Client.swift

/// SPIDependency is visible when using the swiftmodule or private interface.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -experimental-spi-only-imports \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s
// VISIBLE-DEP: loaded module 'SPIDependency'

// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -experimental-spi-only-imports \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s

/// SPIDependency is not visible when using the public swiftinterface.
// RUN: rm %t/Lib.private.swiftinterface
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -experimental-spi-only-imports \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP-NOT %s
// VISIBLE-DEP-NOT-NOT: loaded module 'SPIDependency'

import Lib
