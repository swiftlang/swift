// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/test1.swift -o %t/deps.json -I %t/inputs -Rdependency-scan -serialize-dependency-scan-cache -load-dependency-scan-cache -validate-prior-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -no-parallel-scan &> %t/remarks_initial.txt
// RUN: cat %t/remarks_initial.txt | %FileCheck %s --check-prefix=CHECK-INITIAL

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/test2.swift -o %t/deps2.json -I %t/inputs -Rdependency-scan -serialize-dependency-scan-cache -load-dependency-scan-cache -validate-prior-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -no-parallel-scan &> %t/remarks_second.txt
// RUN: cat %t/remarks_second.txt | %FileCheck %s --check-prefix=CHECK-REUSE

// CHECK-INITIAL: remark: Number of named Clang module queries: '3'
// CHECK-INITIAL: remark: Number of recorded Clang module dependencies queried by-name from a Swift client: '3'
// CHECK-INITIAL: remark: Number of recorded Swift module dependencies: '0'
// CHECK-INITIAL: remark: Number of recorded Clang module dependencies: '3'

// CHECK-REUSE: remark: Number of named Clang module queries: '1'
// CHECK-REUSE: remark: Number of recorded Clang module dependencies queried by-name from a Swift client: '1'
// CHECK-REUSE: remark: Number of recorded Swift module dependencies: '0'
// CHECK-REUSE: remark: Number of recorded Clang module dependencies: '4'

//--- test1.swift
import A
import B
import C
public func test() {}

//--- test2.swift
import A
import B
import C
import D
public func test() {}

//--- inputs/A.h
void b(void);

//--- inputs/B.h
#include "A.h"
void b(void);

//--- inputs/C.h
void c(void);

//--- inputs/D.h
void d(void);

//--- inputs/module.modulemap
module A {
  header "A.h"
  export *
}
module B {
  header "B.h"
  export *
}
module C {
  header "C.h"
  export *
}
module D {
  header "D.h"
  export *
}
