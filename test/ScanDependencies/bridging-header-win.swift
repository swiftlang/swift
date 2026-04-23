// REQUIRES: OS=windows-msvc
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O -module-load-mode prefer-serialized \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -scanner-module-validation \
// RUN:   -I %t %t/test.swift -o %t/deps.json -auto-bridging-header-chaining -scanner-output-dir %t -import-objc-header %t/Bridging.h

// RUN: %{python} %S/../../utils/swift-build-modules.py %swift_frontend_plain %t/deps.json -o %t/Test.cmd -b %t/header.cmd
// RUN: %target-swift-frontend-plain @%t/header.cmd %t/Bridging.h -disable-implicit-swift-modules -O -o %t/bridging.pch

// RUN: %target-swift-frontend-plain -module-name Test -O @%t/Test.cmd %t/test.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -import-objc-header %t/Bridging.h -import-pch %t/bridging.pch \
// RUN:   -emit-module -o %t/Test.swiftmodule

// RUN: %target-swift-frontend -scan-dependencies -module-name User -O -module-load-mode prefer-serialized \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -scanner-module-validation \
// RUN:   -I %t %t/user.swift -import-objc-header %t/Bridging.h -auto-bridging-header-chaining -scanner-output-dir %t \
// RUN:   -o %t/deps2.json

// RUN: %{python} %S/../../utils/swift-build-modules.py %swift_frontend_plain %t/deps2.json -o %t/User.cmd -b %t/header2.cmd

// RUN: %target-swift-frontend-plain @%t/header2.cmd -disable-implicit-swift-modules -O -o %t/bridging2.pch

// RUN: %target-swift-frontend-plain -module-name User -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -import-objc-header %t/Bridging.h -import-pch %t/bridging2.pch \
// RUN:   @%t/User.cmd %t/user.swift \
// RUN:   -emit-module -o %t/User.swiftmodule

//--- test.swift
public func test() {}

//--- user.swift
import Test


//--- Bridging.h
#include "Foo.h"
int Bridge = 0;

//--- Foo.h
int Foo = 0;

