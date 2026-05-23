// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test -print-indexed-symbols -enable-objc-interop -source-filename %t/ObjcUser.swift -Xcc -fmodule-map-file=%t/module.modulemap | %FileCheck -check-prefix=SWIFT %s

//--- objc_decls.h
@interface Test
- (instancetype)init;
@end

@compatibility_alias NEWTest Test;

void testFunc(NEWTest *t);

//--- module.modulemap
module objc_decls {
  header "objc_decls.h"
  export *
}

//--- ObjcUser.swift
import objc_decls
// SWIFT-NOT: NEWTest
func test() {
  let _: NEWTest? = nil
  // SWIFT: 4:10 | class/Swift | Test | c:objc(cs)Test | Ref,Impl,RelCont | rel: 1
  // SWIFT-NOT: NEWTest

  _ = NEWTest()
  // SWIFT: 8:7 | class/Swift | Test | c:objc(cs)Test | Ref,Impl,RelCont | rel: 1
  // SWIFT-NOT: NEWTest
}

class C: NEWTest {}
// SWIFT: 13:10 | class/Swift | Test | c:objc(cs)Test | Ref,Impl,RelBase | rel: 1
// SWIFT-NOT: NEWTest
