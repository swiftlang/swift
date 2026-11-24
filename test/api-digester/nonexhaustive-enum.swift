// RUN: %empty-directory(%t)

// Adding a case to a `@nonexhaustive` enum is not source-breaking: clients in
// other modules can't switch over it exhaustively, so they already have to
// handle unknown cases. This holds even though the module isn't built with
// library evolution enabled.
// RUN: echo "@nonexhaustive public enum E { case a }"           > %t/Foo-1.swift
// RUN: echo "@nonexhaustive public enum E { case a; case b }"   > %t/Foo-2.swift
// RUN: %target-swift-frontend -emit-module %t/Foo-1.swift -module-name Foo -o %t/Foo1.swiftmodule -emit-abi-descriptor-path %t/Foo1.json
// RUN: %target-swift-frontend -emit-module %t/Foo-2.swift -module-name Foo -o %t/Foo2.swiftmodule -emit-abi-descriptor-path %t/Foo2.json
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t/Foo1.json -input-paths %t/Foo2.json -o %t/result-nonexhaustive.txt
// RUN: %FileCheck %s --check-prefix=NONEXHAUSTIVE --allow-empty < %t/result-nonexhaustive.txt

// NONEXHAUSTIVE-NOT: has been added as a new enum case

// The same is true for the staged-in `@nonexhaustive(warn)` spelling.
// RUN: echo "@nonexhaustive(warn) public enum W { case a }"         > %t/Baz-1.swift
// RUN: echo "@nonexhaustive(warn) public enum W { case a; case b }" > %t/Baz-2.swift
// RUN: %target-swift-frontend -emit-module %t/Baz-1.swift -module-name Baz -o %t/Baz1.swiftmodule -emit-abi-descriptor-path %t/Baz1.json
// RUN: %target-swift-frontend -emit-module %t/Baz-2.swift -module-name Baz -o %t/Baz2.swiftmodule -emit-abi-descriptor-path %t/Baz2.json
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t/Baz1.json -input-paths %t/Baz2.json -o %t/result-warn.txt
// RUN: %FileCheck %s --check-prefix=WARN --allow-empty < %t/result-warn.txt

// WARN-NOT: has been added as a new enum case

// An enum without the attribute is still exhaustive here, so adding a case to
// it is still reported.
// RUN: echo "public enum F { case a }"         > %t/Bar-1.swift
// RUN: echo "public enum F { case a; case b }" > %t/Bar-2.swift
// RUN: %target-swift-frontend -emit-module %t/Bar-1.swift -module-name Bar -o %t/Bar1.swiftmodule -emit-abi-descriptor-path %t/Bar1.json
// RUN: %target-swift-frontend -emit-module %t/Bar-2.swift -module-name Bar -o %t/Bar2.swiftmodule -emit-abi-descriptor-path %t/Bar2.json
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t/Bar1.json -input-paths %t/Bar2.json -o %t/result-exhaustive.txt
// RUN: %FileCheck %s --check-prefix=EXHAUSTIVE < %t/result-exhaustive.txt

// EXHAUSTIVE: EnumElement F.b has been added as a new enum case
