// Constant globals should "work" when referencing C-imported constants
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// REQUIRES: rdar146952876
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -primary-file %t/main.swift -parse-as-library -import-bridging-header %t/bridging_header.h -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %t/main.swift -parse-as-library -import-bridging-header %t/bridging_header.h -enable-experimental-feature CompileTimeValues

//--- bridging_header.h

static const int c_integer = 42;
static const long c_long = 42;

struct CStruct {
	int a, b, c, d;
};

//--- main.swift

@const let constGlobal1: Int = Int(c_integer)
@const let constGlobal2: Int = Int(c_long)
@const let constGlobal3: Int = MemoryLayout<CStruct>.size
