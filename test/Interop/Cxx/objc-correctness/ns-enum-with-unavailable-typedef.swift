// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t/include -module-name main
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t/include -module-name main -cxx-interoperability-mode=default
// REQUIRES: objc_interop

//--- include/module.modulemap
module ObjCModule {
    header "header.h"
}

//--- include/header.h
#include <Foundation/Foundation.h>

#define UNAVAILABLE API_UNAVAILABLE(macos)

UNAVAILABLE
typedef NS_ENUM(NSUInteger, MyUnavailableEnum) {
    MyUnavailableEnumCase1 = 0,
    MyUnavailableEnumCase2,
};

UNAVAILABLE
void unavailableParam(MyUnavailableEnum e);

UNAVAILABLE
MyUnavailableEnum unavailableReturn(void);

struct UNAVAILABLE UnavailableStruct {
  MyUnavailableEnum field;
};

UNAVAILABLE
@interface UnavailableClass
@property (nonatomic, readonly) MyUnavailableEnum prop;
@end

UNAVAILABLE
__attribute__((swift_name("getter:UnavailableStruct.iProp(self:)")))
inline MyUnavailableEnum getUnavailableInjProp(struct UnavailableStruct s) {
  return MyUnavailableEnumCase1;
}

//--- main.swift
import Foundation
import ObjCModule

@available(*, unavailable)
func useParam(_ e: MyUnavailableEnum) { unavailableParam(e) }

@available(*, unavailable)
func useReturn() -> MyUnavailableEnum { return unavailableReturn() }

@available(*, unavailable)
func useField(_ o: UnavailableStruct) -> MyUnavailableEnum { return o.field }

@available(*, unavailable)
func useIProp(_ o: UnavailableStruct) -> MyUnavailableEnum { return o.iProp }

@available(*, unavailable)
func useProp(_ o: UnavailableClass) -> MyUnavailableEnum { return o.prop }
