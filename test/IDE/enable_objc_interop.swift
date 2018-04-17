// RUN: %target-swift-ide-test -source-filename %s -annotate 2>&1 >/dev/null | %FileCheck %s -check-prefix=CHECK-%target-runtime

#if _runtime(_ObjC)
foo
// CHECK-objc: unresolved identifier 'foo'
// CHECK-objc-NOT: unresolved identifier 'bar'
// CHECK-objc-NOT: unresolved identifier 'baz'

#elseif _runtime(_Native)
bar
// CHECK-native-NOT: unresolved identifier 'foo'
// CHECK-native: unresolved identifier 'bar'
// CHECK-native-NOT: unresolved identifier 'baz'

#else
baz
#endif