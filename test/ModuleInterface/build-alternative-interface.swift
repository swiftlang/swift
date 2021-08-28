// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/sources)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/alternative-inputs)
// RUN: %empty-directory(%t/module-cache)

// RUN: echo "public func foo() {}" > %t/sources/Foo.swift
// RUN: %target-swift-frontend-typecheck -emit-module-interface-path %t/inputs/Foo.swiftinterface %t/sources/Foo.swift -module-name Foo -disable-implicit-concurrency-module-import -enable-library-evolution -module-cache-path %t/module-cache -I %t/inputs -swift-version 5
// RUN: cp %t/inputs/Foo.swiftinterface %t/alternative-inputs/Foo.swiftinterface

// RUN: echo "import Foo" > %t/sources/Bar.swift
// RUN: echo "public func foo() {}" >> %t/sources/Bar.swift
// RUN: %target-swift-frontend-typecheck -emit-module-interface-path %t/inputs/Bar.swiftinterface %t/sources/Bar.swift -module-name Bar -disable-implicit-concurrency-module-import -enable-library-evolution -module-cache-path %t/module-cache -I %t/inputs -swift-version 5
// RUN: cp %t/inputs/Bar.swiftinterface %t/alternative-inputs/Bar.swiftinterface

// RUN: echo "import Bar" > %t/sources/FooBar.swift
// RUN: echo "public func foo() {}" >> %t/sources/FooBar.swift
// RUN: %target-swift-frontend-typecheck -emit-module-interface-path %t/inputs/FooBar.swiftinterface %t/sources/FooBar.swift -module-name FooBar -disable-implicit-concurrency-module-import -enable-library-evolution -module-cache-path %t/module-cache -I %t/inputs -swift-version 5

// RUN: echo "messmessmess" >> %t/inputs/Foo.swiftinterface
// RUN: echo "messmessmess" >> %t/inputs/Bar.swiftinterface

// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/inputs -backup-module-interface-path %t/alternative-inputs -module-cache-path %t/module-cache

// RUN: touch -t 201401240005 %t/inputs/Bar.swiftinterface
// RUN: %target-swift-frontend-typecheck -disable-implicit-concurrency-module-import -I %t/inputs -backup-module-interface-path %t/alternative-inputs -module-cache-path %t/module-cache -Rmodule-interface-rebuild %s &> %t/remarks.txt
// RUN: %FileCheck --input-file %t/remarks.txt %s --check-prefix=CHECK-REBUILD
// CHECK-REBUILD: remark: rebuilding module 'FooBar' from interface
// CHECK-REBUILD: remark: rebuilding module 'Bar' from interface

// RUN: %target-swift-frontend-typecheck -disable-implicit-concurrency-module-import -I %t/inputs -backup-module-interface-path %t/alternative-inputs -module-cache-path %t/module-cache -Rmodule-interface-rebuild %s &> %t/no-remarks.txt
// RUN: echo "additional" >> %t/no-remarks.txt
// RUN: %FileCheck --input-file %t/no-remarks.txt %s --check-prefix=CHECK-REBUILD-NOT
// CHECK-REBUILD-NOT-NOT: remark

import FooBar
import Foo
import Bar
