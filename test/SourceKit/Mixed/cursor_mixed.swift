
func test(_ b : Base) {
  b.doIt(0);
}

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=cursor -pos=3:7 %s -- %s %mcp_opt -F %S/Inputs -module-name Mixed -import-underlying-module | %FileCheck %s

// CHECK: source.lang.swift.ref.function.method.instance ({{.*}}Mixed.framework/Headers/Mixed.h:5:9-5:23)
// CHECK: doIt(_:)
// CHECK: c:objc(cs)Base(im)doIt:
// CHECK: (Base) -> (Int32) -> ()
// CHECK: Mixed
// CHECK: <Declaration>func doIt(_ arg: <Type usr="s:s5Int32V">Int32</Type>)</Declaration>
