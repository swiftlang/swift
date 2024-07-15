// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test %t/main.swift -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps-1.json -I %t/include

// RUN: %target-swift-frontend -scan-dependencies -module-name Test %t/main.swift -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps-2.json -Xcc -DHAS_FOO=1 -I %t/include

// RUN: %target-swift-frontend -scan-dependencies -module-name Test %t/main.swift -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps-3.json -Xcc -fapplication-extension -I %t/include

/// Check module hash for the swiftmodule. They should all not match.
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps-1.json Library modulePath > %t/path-1
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps-2.json Library modulePath > %t/path-2
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps-3.json Library modulePath > %t/path-3
// RUN: not diff %t/path-1 %t/path-2
// RUN: not diff %t/path-1 %t/path-3

/// Check build command (exclude dependency module file path). 1 and 2 should match, but not 3.
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-1.json Library | grep -v fmodule-file= > %t/lib-1.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-2.json Library | grep -v fmodule-file= > %t/lib-2.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-3.json Library | grep -v fmodule-file= > %t/lib-3.cmd
// RUN: not diff %t/lib-1.cmd %t/lib-2.cmd
// RUN: not diff %t/lib-1.cmd %t/lib-3.cmd

/// Test direct-cc1 mode.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test %t/main.swift -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps-4.json -I %t/include -experimental-clang-importer-direct-cc1-scan
// RUN: %target-swift-frontend -scan-dependencies -module-name Test %t/main.swift -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps-5.json -Xcc -DHAS_FOO=1 -I %t/include -experimental-clang-importer-direct-cc1-scan
// RUN: %target-swift-frontend -scan-dependencies -module-name Test %t/main.swift -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps-6.json -Xcc -fapplication-extension -I %t/include -experimental-clang-importer-direct-cc1-scan

// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps-4.json Library modulePath > %t/path-4
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps-5.json Library modulePath > %t/path-5
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps-6.json Library modulePath > %t/path-6
// RUN: not diff %t/path-4 %t/path-5
// RUN: not diff %t/path-4 %t/path-6
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-4.json Library | grep -v fmodule-file= > %t/lib-4.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-5.json Library | grep -v fmodule-file= > %t/lib-5.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-6.json Library | grep -v fmodule-file= > %t/lib-6.cmd
// RUN: not diff %t/lib-4.cmd %t/lib-5.cmd
// RUN: not diff %t/lib-4.cmd %t/lib-6.cmd

/// interface doesn't build due to failed type-check.
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-1.json clang:SwiftShims > %t/shim1.cmd
// RUN: %swift_frontend_plain @%t/shim1.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-1.json clang:A > %t/a1.cmd
// RUN: %swift_frontend_plain @%t/a1.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-1.json Library > %t/lib1.cmd
// RUN: not %swift_frontend_plain @%t/lib1.cmd

/// interface builds if clang module has the correct preprocessor defines.
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-2.json clang:SwiftShims > %t/shim2.cmd
// RUN: %swift_frontend_plain @%t/shim2.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-2.json clang:A > %t/a2.cmd
// RUN: %swift_frontend_plain @%t/a2.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps-2.json Library > %t/lib2.cmd
// RUN: %swift_frontend_plain @%t/lib2.cmd

/// This should fail no matter if the swiftmodule is already compiled or not.
// RUN: not %swift_frontend_plain @%t/lib1.cmd

//--- main.swift
import Library

//--- include/Library.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Library -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -user-module-version 1.0
import Swift
@_exported import A
@inlinable
public func test() {
    foo()
}

//--- include/a.h
#ifdef HAS_FOO
void foo(void);
#endif
void bar(void);

//--- include/module.modulemap
module A {
  header "a.h"
  export *
}
