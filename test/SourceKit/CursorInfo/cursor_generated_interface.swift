// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/frameworks/LibA.framework/Modules/LibA.swiftmodule %t/mods %t/mods
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -module-name LibB -emit-module -emit-module-path %t/mods/LibB.swiftmodule -emit-module-source-info-path %t/mods/LibB.swiftsourceinfo %t/libB.swift
// RUN: %target-swift-frontend -module-name LibC -emit-module -emit-module-path %t/mods/LibC.swiftmodule %t/libC.swift
// RUN: %target-swift-frontend -module-name LibA -emit-module -emit-module-path %t/frameworks/LibA.framework/Modules/LibA.swiftmodule/%target-swiftmodule-name -import-underlying-module -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -F %t/frameworks -I %t/mods %t/libA.swift
// RUN: %swift-ide-test -print-module -print-interface -source-filename dummy -module-to-print LibA -F %t/frameworks -target %target-triple &> %t/generated.swift

// Check that we always include module name, regardless of whether we have
// source information or not. If we have source information, we should also
// output it.

//--- use.swift

import LibA

// RUN: %sourcekitd-test -req=cursor -pos=%(line+1):8 -print-raw-response %t/use.swift -- -F %t/frameworks -target %target-triple %t/use.swift | %FileCheck %s --check-prefix=CHECKSWIFT
let _: ASwiftType
// CHECKSWIFT: key.name: "ASwiftType"
// CHECKSWIFT: key.modulename: "LibA"
// CHECKSWIFT: key.decl_lang: source.lang.swift

// RUN: %sourcekitd-test -req=cursor -pos=%(line+1):8 -print-raw-response %t/use.swift -- -F %t/frameworks -target %target-triple %t/use.swift | %FileCheck %s --check-prefix=CHECKOBJC
let _: AObjcType
// CHECKOBJC: key.name: "AObjcType"
// CHECKOBJC: key.modulename: "LibA"
// CHECKOBJC: key.decl_lang: source.lang.objc

//--- libA.swift

import LibB
import LibC
import LibD

public class ASwiftType {
  public func aTypes(swift: ASwiftType, objc: AObjcType, sub: ASubType) {}
  public func others(b: BType, c: CType, d: DType) {}
}

// LibA is a mixed framework with no source info and a submodule
// RUN: %sourcekitd-test -req=interface-gen-open -module LibA -- -F %t/frameworks -target %target-triple == -req=cursor -pos=11:36 -print-raw-response | %FileCheck %s --check-prefix=CHECKA
// CHECKA: key.name: "ASwiftType"
// CHECKA: key.modulename: "LibA"
// CHECKA: key.decl_lang: source.lang.swift

//--- frameworks/LibA.framework/Modules/module.modulemap
framework module LibA {
  header "LibA.h"
  export *

  module Sub {
    header "LibASub.h"
  }
}

//--- frameworks/LibA.framework/Headers/LibA.h
@interface AObjcType
@end

// RUN: %sourcekitd-test -req=interface-gen-open -module LibA -- -F %t/frameworks -target %target-triple == -req=cursor -pos=11:54 -print-raw-response | %FileCheck %s --check-prefix=CHECKAOBJ
// CHECKAOBJ: key.name: "AObjcType"
// CHECKAOBJ: key.line: [[@LINE-5]]
// CHECKAOBJ: key.column: 12
// CHECKAOBJ: key.filepath: {{.*}}LibA.h
// CHECKAOBJ: key.modulename: "LibA"
// CHECKAOBJ: key.decl_lang: source.lang.objc

//--- frameworks/LibA.framework/Headers/LibASub.h
@interface ASubType
@end

// RUN: %sourcekitd-test -req=interface-gen-open -module LibA -- -F %t/frameworks -target %target-triple == -req=cursor -pos=11:70 -print-raw-response | %FileCheck %s --check-prefix=CHECKASUB
// CHECKASUB: key.name: "ASubType"
// CHECKASUB: key.line: [[@LINE-5]]
// CHECKASUB: key.column: 12
// CHECKASUB: key.filepath: {{.*}}LibASub.h
// CHECKASUB: key.modulename: "LibA.Sub"
// CHECKASUB: key.decl_lang: source.lang.objc

//--- libB.swift
public class BType {}

// LibB has source info
// RUN: %sourcekitd-test -req=interface-gen-open -module LibA -- -F %t/frameworks -target %target-triple == -req=cursor -pos=13:32 -print-raw-response | %FileCheck %s --check-prefix=CHECKB
// CHECKB: key.name: "BType"
// CHECKB: key.line: [[@LINE-5]]
// CHECKB: key.column: 14
// CHECKB: key.filepath: {{.*}}libB.swift
// CHECKB: key.modulename: "LibB"
// CHECKB: key.decl_lang: source.lang.swift

//--- libC.swift
public class CType {}

// LibC has no source info
// RUN: %sourcekitd-test -req=interface-gen-open -module LibA -- -F %t/frameworks -target %target-triple == -req=cursor -pos=13:47 -print-raw-response | %FileCheck %s --check-prefix=CHECKC
// CHECKC: key.name: "CType"
// CHECKC: key.modulename: "LibC"
// CHECKC: key.decl_lang: source.lang.swift

//--- mods/LibD.h
@interface DType
@end

// RUN: %sourcekitd-test -req=interface-gen-open -module LibA -- -F %t/frameworks -target %target-triple == -req=cursor -pos=13:57 -print-raw-response | %FileCheck %s --check-prefix=CHECKD
// CHECKD: key.name: "DType"
// CHECKD: key.line: [[@LINE-5]]
// CHECKD: key.column: 12
// CHECKD: key.filepath: {{.*}}LibD.h
// CHECKD: key.modulename: "LibD"
// CHECKD: key.decl_lang: source.lang.objc

//--- mods/module.modulemap
module LibD {
  header "LibD.h"
  export *
}
