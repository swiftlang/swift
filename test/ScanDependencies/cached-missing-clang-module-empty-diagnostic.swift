// When multiple Swift modules transitively import the same missing Clang
// module, clang's loadModule cache returns subsequent lookups without
// buffering a diagnostic. The resulting empty messages should be suppressed
// rather than emitted as standalone `clang dependency scanning failure:`
// errors.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %empty-directory(%t/build)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/build/SwiftFoo1.swiftmodule -module-name SwiftFoo1 -I %t/cfoo -Xcc -fmodule-map-file=%t/cfoo/module.modulemap %t/SwiftFoo1.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/build/SwiftFoo2.swiftmodule -module-name SwiftFoo2 -I %t/cfoo -Xcc -fmodule-map-file=%t/cfoo/module.modulemap %t/SwiftFoo2.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/build/SwiftFoo3.swiftmodule -module-name SwiftFoo3 -I %t/cfoo -Xcc -fmodule-map-file=%t/cfoo/module.modulemap %t/SwiftFoo3.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/build/SwiftFoo4.swiftmodule -module-name SwiftFoo4 -I %t/cfoo -Xcc -fmodule-map-file=%t/cfoo/module.modulemap %t/SwiftFoo4.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/build/SwiftFoo5.swiftmodule -module-name SwiftFoo5 -I %t/cfoo -Xcc -fmodule-map-file=%t/cfoo/module.modulemap %t/SwiftFoo5.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/build/SwiftFoo6.swiftmodule -module-name SwiftFoo6 -I %t/cfoo -Xcc -fmodule-map-file=%t/cfoo/module.modulemap %t/SwiftFoo6.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/build/SwiftFoo7.swiftmodule -module-name SwiftFoo7 -I %t/cfoo -Xcc -fmodule-map-file=%t/cfoo/module.modulemap %t/SwiftFoo7.swift

// RUN: %target-swift-frontend -scan-dependencies %t/client.swift -I %t/build -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import &> %t/output.txt
// RUN: %FileCheck %s < %t/output.txt

// CHECK: error: unable to resolve module dependency: 'CFoo'
// CHECK-NOT: error: clang dependency scanning failure:

//--- cfoo/CFoo.h
int cfoo_answer(void);

//--- cfoo/module.modulemap
module CFoo { header "CFoo.h" export * }

//--- SwiftFoo1.swift
import CFoo
//--- SwiftFoo2.swift
import CFoo
//--- SwiftFoo3.swift
import CFoo
//--- SwiftFoo4.swift
import CFoo
//--- SwiftFoo5.swift
import CFoo
//--- SwiftFoo6.swift
import CFoo
//--- SwiftFoo7.swift
import CFoo

//--- client.swift
import SwiftFoo1
import SwiftFoo2
import SwiftFoo3
import SwiftFoo4
import SwiftFoo5
import SwiftFoo6
import SwiftFoo7
