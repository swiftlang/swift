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
func test() {
  let _: NEWTest? = nil
  // SWIFT: 3:10 | class/Swift | Test | c:objc(cs)Test | Ref,RelCont | rel: 1

  _ = NEWTest()
  // SWIFT: 6:7 | class/Swift | Test | c:objc(cs)Test | Ref,RelCont | rel: 1
}

class C: NEWTest {}
// SWIFT: 10:10 | class/Swift | Test | c:objc(cs)Test | Ref,RelBase | rel: 1
