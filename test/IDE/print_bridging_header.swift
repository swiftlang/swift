// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test -print-module -module-to-print=__ObjC -source-filename x -import-objc-header %t/bridge.h > %t/bridge.swiftinterface
// RUN: %diff %t/bridge.swiftinterface %t/bridge.swiftinterface.expected

// -print-module recognizes the special __ObjC module name and maps it to the
// bridging header passed via -import-objc-header, printing that header's
// interface.

//--- bridge.h
void foo(int x);
//--- bridge.swiftinterface.expected
func foo(_ x: CInt)
