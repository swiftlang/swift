// RUN: %empty-directory(%t)

// 1. Not finding things is okay.
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s 2>&1 | %FileCheck -check-prefix=NO-SUCH-MODULE %s
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s 2>&1 | %FileCheck -check-prefix=NO-SUCH-MODULE %s
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s 2>&1 | %FileCheck -check-prefix=NO-SUCH-MODULE %s
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s 2>&1 | %FileCheck -check-prefix=NO-SUCH-MODULE %s
// (default)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s 2>&1 | %FileCheck -check-prefix=NO-SUCH-MODULE %s

// 2. Only interface is present.
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %S/Inputs/force-module-loading-mode/ 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %S/Inputs/force-module-loading-mode/ 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %S/Inputs/force-module-loading-mode/ 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %S/Inputs/force-module-loading-mode/ 2>&1 | %FileCheck -check-prefix=NO-SUCH-MODULE %s
// (default)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -I %S/Inputs/force-module-loading-mode/ %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s

// 3. Only module is present.
// RUN: sed -e 's/FromInterface/FromSerialized/g' %S/Inputs/force-module-loading-mode/Lib.swiftinterface | %target-swift-frontend -parse-stdlib -module-cache-path %t/MCP -emit-module-path %t/Lib.swiftmodule - -module-name Lib
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-SERIALIZED %s
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-SERIALIZED %s
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=NO-SUCH-MODULE %s
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-SERIALIZED %s
// (default)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -I %t %s 2>&1 | %FileCheck -check-prefix=FROM-SERIALIZED %s

// 4. Both are present.
// RUN: cp %S/Inputs/force-module-loading-mode/Lib.swiftinterface %t
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: %empty-directory(%t/MCP)
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-SERIALIZED %s
// RUN: %empty-directory(%t/MCP)
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: %empty-directory(%t/MCP)
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-SERIALIZED %s
// RUN: %empty-directory(%t/MCP)
// (default)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -I %t %s 2>&1 | %FileCheck -check-prefix=FROM-SERIALIZED %s
// RUN: %empty-directory(%t/MCP)

// 5. Both are present but the module is invalid.
// RUN: rm %t/Lib.swiftmodule && touch %t/Lib.swiftmodule
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: %empty-directory(%t/MCP)
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: %empty-directory(%t/MCP)
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: %empty-directory(%t/MCP)
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=BAD-MODULE %s
// RUN: %empty-directory(%t/MCP)
// (default)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -I %t %s 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: %empty-directory(%t/MCP)

// 6. Both are present but the module can't be opened.
// RUN: %{python} %S/../Inputs/make-unreadable.py %t/Lib.swiftmodule
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: %empty-directory(%t/MCP)
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=NO-SUCH-MODULE %s
// RUN: %empty-directory(%t/MCP)
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-parseable not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=FROM-INTERFACE %s
// RUN: %empty-directory(%t/MCP)
// RUN: env SWIFT_FORCE_MODULE_LOADING=only-serialized not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP %s -I %t 2>&1 | %FileCheck -check-prefix=NO-SUCH-MODULE %s
// RUN: %empty-directory(%t/MCP)
// (default)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/MCP -I %t %s 2>&1 | %FileCheck -check-prefix=NO-SUCH-MODULE %s
// RUN: %empty-directory(%t/MCP)

import Lib
// NO-SUCH-MODULE: [[@LINE-1]]:8: error: no such module 'Lib'
// BAD-MODULE: [[@LINE-2]]:8: error: malformed compiled module: {{.*}}Lib.swiftmodule

struct X {}
let _: X = Lib.testValue
// FROM-INTERFACE: [[@LINE-1]]:16: error: cannot convert value of type 'FromInterface' to specified type 'X'
// FROM-SERIALIZED: [[@LINE-2]]:16: error: cannot convert value of type 'FromSerialized' to specified type 'X'
