// RUN: %target-swift-ide-test -source-filename %s -annotate 2>&1 >/dev/null | %FileCheck %s -check-prefix=CHECK-%target-runtime

#if _runtime(_ObjC)
foo
// CHECK-objc: cannot find 'foo' in scope
// CHECK-objc-NOT: cannot find 'bar' in scope
// CHECK-objc-NOT: cannot find 'baz' in scope

#elseif _runtime(_Native)
bar
// CHECK-native-NOT: cannot find 'foo' in scope
// CHECK-native: cannot find 'bar' in scope
// CHECK-native-NOT: cannot find 'baz' in scope

#else
baz
#endif
