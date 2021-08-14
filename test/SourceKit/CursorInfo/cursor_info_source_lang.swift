import Foo
import Module

class FromSwift {}
@objc class StillFromSwift {}
func test(fromObjC: FooClassDerived,
          _: FromSwiftMod,
          _: FromSwiftModObjC) {}

// SWIFT: source.lang.swift{{$}}
// OBJC: source.lang.objc{{$}}

// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: echo '// swift-interface-format-version: 1.0' > %t/Module.swiftinterface
// RUN: echo '// swift-module-flags: -swift-version 5 -enable-library-evolution -module-name Module' >> %t/Module.swiftinterface
// RUN: echo ' public class FromSwiftMod {}' >> %t/Module.swiftinterface
// RUN: echo ' @objc public class FromSwiftModObjC {}' >> %t/Module.swiftinterface

// RUN: %sourcekitd-test -req=cursor -pos=4:7 %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck -check-prefix=SWIFT %s
// RUN: %sourcekitd-test -req=cursor -pos=5:13 %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck -check-prefix=SWIFT %s
// RUN: %sourcekitd-test -req=cursor -pos=6:21 %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck -check-prefix=OBJC %s
// RUN: %sourcekitd-test -req=cursor -pos=7:14 %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck -check-prefix=SWIFT %s
// RUN: %sourcekitd-test -req=cursor -pos=8:14 %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck -check-prefix=SWIFT %s
