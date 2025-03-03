// Constant globals should "work" when referencing C-imported constants

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -primary-file %t/main.swift -parse-as-library -import-bridging-header %t/bridging_header.h

//--- bridging_header.h

static const int c_integer = 42;
static const long c_long = 42;

struct CStruct {
	int a, b, c, d;
};

//--- main.swift

_const let constGlobal1: Int = Int(c_integer)
_const let constGlobal2: Int = Int(c_long)
_const let constGlobal3: Int = MemoryLayout<CStruct>.size
