// REQUIRES: VENDOR=apple

// Search paths should be honored in command-line order, like the frontend
// does, whether or not they are system paths.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/User/Foo.framework/Modules/Foo.swiftmodule)
// RUN: %empty-directory(%t/System/Foo.framework/Modules/Foo.swiftmodule)
// RUN: %empty-directory(%t/UserImport)
// RUN: %empty-directory(%t/SystemImport)
// RUN: %empty-directory(%t/module-cache)

// RUN: echo "public func fromUser() {}" > %t/User.swift
// RUN: echo "public func fromSystem() {}" > %t/System.swift

// RUN: %target-swift-frontend -emit-module %t/User.swift -module-name Foo -parse-as-library %clang-importer-sdk-nosource -o %t/User/Foo.framework/Modules/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -emit-module %t/System.swift -module-name Foo -parse-as-library %clang-importer-sdk-nosource -o %t/System/Foo.framework/Modules/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -emit-module %t/User.swift -module-name Foo -parse-as-library %clang-importer-sdk-nosource -o %t/UserImport/Foo.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/System.swift -module-name Foo -parse-as-library %clang-importer-sdk-nosource -o %t/SystemImport/Foo.swiftmodule

// -F and -Fsystem
// RUN: %api-digester -dump-sdk -module Foo -sdk %clang-importer-sdk-path -module-cache-path %t/module-cache -swift-only -avoid-tool-args -abort-on-module-fail -o %t/dump.json -F %t/User -Fsystem %t/System
// RUN: %FileCheck %s --check-prefix=USER --implicit-check-not=fromSystem < %t/dump.json
// RUN: %api-digester -dump-sdk -module Foo -sdk %clang-importer-sdk-path -module-cache-path %t/module-cache -swift-only -avoid-tool-args -abort-on-module-fail -o %t/dump.json -Fsystem %t/System -F %t/User
// RUN: %FileCheck %s --check-prefix=SYSTEM --implicit-check-not=fromUser < %t/dump.json

// -I and -Isystem
// RUN: %api-digester -dump-sdk -module Foo -sdk %clang-importer-sdk-path -module-cache-path %t/module-cache -swift-only -avoid-tool-args -abort-on-module-fail -o %t/dump.json -I %t/UserImport -Isystem %t/SystemImport
// RUN: %FileCheck %s --check-prefix=USER --implicit-check-not=fromSystem < %t/dump.json
// RUN: %api-digester -dump-sdk -module Foo -sdk %clang-importer-sdk-path -module-cache-path %t/module-cache -swift-only -avoid-tool-args -abort-on-module-fail -o %t/dump.json -Isystem %t/SystemImport -I %t/UserImport
// RUN: %FileCheck %s --check-prefix=SYSTEM --implicit-check-not=fromUser < %t/dump.json

// USER: "printedName": "fromUser()"
// SYSTEM: "printedName": "fromSystem()"

// -Fsystem is shared by the baseline (-BF) and the current (-F) search paths.
// RUN: %api-digester -diagnose-sdk -module Foo -sdk %clang-importer-sdk-path -bsdk %clang-importer-sdk-path -module-cache-path %t/module-cache -swift-only -abort-on-module-fail -o %t/result.txt -BF %t/User -Fsystem %t/System -F %t/User
// RUN: %FileCheck %s --check-prefix=BASELINE-USER --implicit-check-not=fromSystem < %t/result.txt
// RUN: %api-digester -diagnose-sdk -module Foo -sdk %clang-importer-sdk-path -bsdk %clang-importer-sdk-path -module-cache-path %t/module-cache -swift-only -abort-on-module-fail -o %t/result.txt -F %t/User -Fsystem %t/System -BF %t/User
// RUN: %FileCheck %s --check-prefix=BASELINE-SYSTEM --implicit-check-not=fromUser < %t/result.txt

// -Fsystem alone doesn't make the baseline a module to load, so -baseline-path is still used.
// RUN: %api-digester -dump-sdk -module Foo -sdk %clang-importer-sdk-path -module-cache-path %t/module-cache -swift-only -avoid-tool-args -abort-on-module-fail -o %t/user.json -F %t/User
// RUN: %api-digester -diagnose-sdk -module Foo -sdk %clang-importer-sdk-path -module-cache-path %t/module-cache -swift-only -abort-on-module-fail -o %t/result.txt -baseline-path %t/user.json -Fsystem %t/System -F %t/User
// RUN: %FileCheck %s --check-prefix=BASELINE-USER --implicit-check-not=fromSystem < %t/result.txt

// BASELINE-USER: Func fromUser() has been removed
// BASELINE-SYSTEM: Func fromSystem() has been removed
