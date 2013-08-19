// RUN: %swift-ide-test -dynamic-lookup-completion -source-filename %s > %t.txt
// RUN: FileCheck %s < %t.txt
// RUN: FileCheck %s -check-prefix=NEGATIVE < %t.txt
// REQUIRES: sdk

import ObjectiveC

// CHECK: Begin members
// CHECK-DAG: func [objc] class() -> Class
// CHECK-DAG: func [objc] performSelector(aSelector : SEL) -> id
// CHECK-DAG: func [objc] conformsToProtocol(aProtocol : Protocol) -> Bool
// CHECK-DAG: func [objc] isKindOfClass(aClass : Class) -> Bool
// CHECK: End members


// No class functions
// NEGATIVE-NOT: isSubclassOfClass

// Nothing marked unavailable
// NEGATIVE-NOT: [objc] retain()
// NEGATIVE-NOT: [objc] release()
