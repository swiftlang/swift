// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name SwiftName -F %t/frameworks -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/SwiftName.symbols.json

//--- frameworks/SwiftName.framework/Modules/module.modulemap
framework module SwiftName {
  umbrella header "SwiftName.h"
  export *
  module * { export * }
}

//--- frameworks/SwiftName.framework/Headers/SwiftName.h
#import "OtherHeader.h"

typedef struct {
    double val;
} MyDouble;

// The swift_name attribute below generates extension decls in both header modules, which trips an
// assertion if they are both added in getDisplayDecls. Make sure that this does not crash when a
// symbol graph is generated.

// CHECK-DAG: "precise": "c:SwiftName.h@MyDoubleFixedValue"
__attribute__((swift_name("MyDouble.FixedValue")))
static double MyDoubleFixedValue = 0.0;

//--- frameworks/SwiftName.framework/Headers/OtherHeader.h
// CHECK-DAG: "precise": "c:OtherHeader.h@myVar"
static int myVar = 0;

