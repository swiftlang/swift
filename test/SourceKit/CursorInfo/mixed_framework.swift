// RUN: %empty-directory(%t.mcp)

public class MySwiftType {
    public func bar(x: MyObjcType) {
// Check that we don't crash
// RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):11 -req-opts=retrieve_symbol_graph=1 %s -- -module-name MixedFramework -F %S/Inputs/mixed_framework -import-underlying-module -module-cache-path %t.mcp %s
        x.foo()
    }
}
