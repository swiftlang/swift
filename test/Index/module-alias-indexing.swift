// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend %t/FileLogging.swift -module-name AppleLogging -module-alias XLogging=AppleLogging -emit-module -o %t/AppleLogging.swiftmodule -index-store-path %t/indexResult
// RUN: %target-swift-frontend -typecheck %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t -index-store-path %t/indexResult

// RUN: c-index-test core -print-unit %t/indexResult > %t/indexUnitDump.txt
// RUN: %FileCheck %s -input-file %t/indexUnitDump.txt -check-prefix CHECK-UNIT

// RUN: c-index-test core -print-record %t/indexResult > %t/indexRecordDump.txt
// RUN: %FileCheck %s -input-file %t/indexRecordDump.txt -check-prefix CHECK-RECORD

// BEGIN FileLogging.swift
public struct Logger {
  public init() {}
}

public func setup() -> XLogging.Logger? {
  return Logger()
}

// BEGIN FileLib.swift
import XLogging

public func start() {
  _ = XLogging.setup()
}


// CHECK-UNIT: [[MODA:--[A-Z0-9]*]]
// CHECK-UNIT: --------
// CHECK-UNIT: module-name: FileLib
// CHECK-UNIT: has-main: 1
// CHECK-UNIT: main-path: {{.*}}{{/|\\}}FileLib.swift
// CHECK-UNIT: out-file: -
// CHECK-UNIT: DEPEND START
// CHECK-UNIT: Unit | system | Swift | {{.*}}{{/|\\}}Swift.swiftmodule
// CHECK-UNIT: Unit | user | AppleLogging | {{.*}}{{/|\\}}AppleLogging.swiftmodule
// CHECK-UNIT: Record | user | {{.*}}{{/|\\}}FileLib.swift | [[MODA:FileLib.swift-[A-Z0-9]*]]
// CHECK-UNIT: DEPEND END

// CHECK-UNIT: [[MODA:AppleLogging.swiftmodule-[A-Z0-9]*]]
// CHECK-UNIT: --------
// CHECK-UNIT: module-name: AppleLogging
// CHECK-UNIT: has-main: 1
// CHECK-UNIT: main-path: {{.*}}{{/|\\}}FileLogging.swift
// CHECK-UNIT: out-file: {{.*}}{{/|\\}}AppleLogging.swiftmodule
// CHECK-UNIT: DEPEND START
// CHECK-UNIT: Unit | system | Swift | {{.*}}{{/|\\}}Swift.swiftmodule
// CHECK-UNIT: Record | user | {{.*}}{{/|\\}}FileLogging.swift | [[MODA:FileLogging.swift-[A-Z0-9]*]]
// CHECK-UNIT: DEPEND END

// CHECK-RECORD: FileLib.swift
// CHECK-RECORD: ------------
// CHECK-RECORD: module/Swift | AppleLogging | c:@M@AppleLogging | <no-cgname> | Ref,RelCont -
// CHECK-RECORD: function/Swift | start() | s:7FileLib5startyyF | <no-cgname> | Def - RelCall,RelCont
// CHECK-RECORD: function/Swift | setup() | s:12AppleLogging5setupAA6LoggerVSgyF | <no-cgname> | Ref,Call,RelCall,RelCont -
// CHECK-RECORD: ------------
// CHECK-RECORD: 1:8 | module/Swift | c:@M@AppleLogging | Ref | rel: 0
// CHECK-RECORD: 3:13 | function/Swift | s:7FileLib5startyyF | Def | rel: 0
// CHECK-RECORD: 4:7 | module/Swift | c:@M@AppleLogging | Ref,RelCont | rel: 1
// CHECK-RECORD:   RelCont | s:7FileLib5startyyF
// CHECK-RECORD: 4:16 | function/Swift | s:12AppleLogging5setupAA6LoggerVSgyF | Ref,Call,RelCall,RelCont | rel: 1
// CHECK-RECORD:   RelCall,RelCont | s:7FileLib5startyyF

// CHECK-RECORD: FileLogging.swift
// CHECK-RECORD: ------------
// CHECK-RECORD: struct/Swift | Logger | s:12AppleLogging6LoggerV | <no-cgname> | Def,Ref,RelCont - RelChild
// CHECK-RECORD: constructor/Swift | init() | s:12AppleLogging6LoggerVACycfc | <no-cgname> | Def,Ref,Call,RelChild,RelCall,RelCont -
// CHECK-RECORD: function/Swift | setup() | s:12AppleLogging5setupAA6LoggerVSgyF | <no-cgname> | Def - RelCall,RelCont
// CHECK-RECORD: ------------
// CHECK-RECORD: 1:15 | struct/Swift | s:12AppleLogging6LoggerV | Def | rel: 0
// CHECK-RECORD: 2:10 | constructor/Swift | s:12AppleLogging6LoggerVACycfc | Def,RelChild | rel: 1
// CHECK-RECORD:   RelChild | s:12AppleLogging6LoggerV
// CHECK-RECORD: 5:13 | function/Swift | s:12AppleLogging5setupAA6LoggerVSgyF | Def | rel: 0
// CHECK-RECORD: 5:24 | module/Swift | c:@M@AppleLogging | Ref,RelCont | rel: 1
// CHECK-RECORD:   RelCont | s:12AppleLogging5setupAA6LoggerVSgyF
// CHECK-RECORD: 5:33 | struct/Swift | s:12AppleLogging6LoggerV | Ref,RelCont | rel: 1
// CHECK-RECORD:   RelCont | s:12AppleLogging5setupAA6LoggerVSgyF
// CHECK-RECORD: 6:10 | struct/Swift | s:12AppleLogging6LoggerV | Ref,RelCont | rel: 1
// CHECK-RECORD:   RelCont | s:12AppleLogging5setupAA6LoggerVSgyF
// CHECK-RECORD: 6:10 | constructor/Swift | s:12AppleLogging6LoggerVACycfc | Ref,Call,RelCall,RelCont | rel: 1
// CHECK-RECORD:   RelCall,RelCont | s:12AppleLogging5setupAA6LoggerVSgyF

