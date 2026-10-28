// REQUIRES: swift_feature_CustomAvailability

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %t/library.swift \
// RUN:   -I %t -enable-experimental-feature CustomAvailability \
// RUN:   -define-availability "AvailMacro:DynamicDomain" -parse-as-library \
// RUN:   | %FileCheck -dump-input=always %t/library.swift

// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %t/main.swift \
// RUN:   -I %t -enable-experimental-feature CustomAvailability \
// RUN:   -define-availability "AvailMacro:DynamicDomain" \
// RUN:   | %FileCheck -dump-input=always %t/main.swift

// RUN: %target-swift-frontend -emit-module -o %t/Library.swiftmodule %t/library.swift \
// RUN:   -module-name Library -I %t -enable-experimental-feature CustomAvailability \
// RUN:   -define-availability "AvailMacro:DynamicDomain" -parse-as-library
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %t/main.swift \
// RUN:   -module-to-print Library -I %t -enable-experimental-feature CustomAvailability \
// RUN:   | %FileCheck -dump-input=always -check-prefix=MODULE %t/library.swift

// A module inside the SDK is a system module.
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %t/main.swift \
// RUN:   -module-to-print Library -I %t -enable-experimental-feature CustomAvailability \
// RUN:   -sdk %t | %FileCheck -dump-input=always -check-prefix=SYSTEM %t/library.swift

//--- module.modulemap
module AvailabilityDomains {
  header "availability_domains.h"
  export *
}

//--- availability_domains.h
#include <availability_domain.h>

int dynamic_domain_pred(void);

CLANG_DYNAMIC_AVAILABILITY_DOMAIN(DynamicDomain, dynamic_domain_pred);
CLANG_ENABLED_AVAILABILITY_DOMAIN(EnabledDomain);

//--- library.swift
import AvailabilityDomains

// SYSTEM-NOT: __clang_availability_domain

@available(macOS 99, iOS 99, tvOS 99, watchOS 99, visionOS 99, *)
// CHECK-NOT: availability_domain
public func platformsOnly() {}
// CHECK: [[@LINE-1]]:13 | function(public)/Swift | platformsOnly()
// CHECK-NOT: availability_domain
// MODULE-NOT: availability_domain
// MODULE: 0:0 | function(public)/Swift | platformsOnly()
// MODULE-NOT: availability_domain

@available(DynamicDomain)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | availableInDynamicDomain()
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | function/Swift | availableInDynamicDomain()
// MODULE-NEXT: 0:0 | function(public)/Swift | availableInDynamicDomain()
public func availableInDynamicDomain() {}

// Unavailable declarations in a binary module are not indexed.
@available(EnabledDomain, unavailable)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_EnabledDomain | c:availability_domains.h@__clang_availability_domain_EnabledDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | unavailableInEnabledDomain()
// MODULE-NOT: unavailableInEnabledDomain()
public func unavailableInEnabledDomain() {}

// Attributes are visited in reverse source order.
@available(macOS 99, *)
@available(DynamicDomain)
@available(EnabledDomain)
public func multipleAttributes() {}
// CHECK: [[@LINE-2]]:12 | variable/Swift | __clang_availability_domain_EnabledDomain | c:availability_domains.h@__clang_availability_domain_EnabledDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | multipleAttributes()
// CHECK: [[@LINE-5]]:12 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | multipleAttributes()
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_EnabledDomain | c:availability_domains.h@__clang_availability_domain_EnabledDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | function/Swift | multipleAttributes()
// MODULE-NEXT: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | function/Swift | multipleAttributes()
// MODULE-NEXT: 0:0 | function(public)/Swift | multipleAttributes()

@available(AvailMacro)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | fromAttributeMacro()
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | function/Swift | fromAttributeMacro()
// MODULE-NEXT: 0:0 | function(public)/Swift | fromAttributeMacro()
public func fromAttributeMacro() {}

// Indexing a binary module reports each global variable twice.
@available(DynamicDomain)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | globalVar
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | variable/Swift | globalVar |
// MODULE-NEXT: 0:0 | variable(public)/Swift | globalVar |
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | variable/Swift | globalVar |
// MODULE-NEXT: 0:0 | variable(public)/Swift | globalVar |
public var globalVar = 1

@available(DynamicDomain)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | globalVar1
// CHECK: [[@LINE-3]]:12 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | globalVar2
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | variable/Swift | globalVar1
// MODULE-NEXT: 0:0 | variable(public)/Swift | globalVar1
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | variable/Swift | globalVar2
// MODULE-NEXT: 0:0 | variable(public)/Swift | globalVar2
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | variable/Swift | globalVar1
// MODULE-NEXT: 0:0 | variable(public)/Swift | globalVar1
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | variable/Swift | globalVar2
// MODULE-NEXT: 0:0 | variable(public)/Swift | globalVar2
public var globalVar1 = 1, globalVar2 = 2

@available(AvailMacro)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | computedGlobalVar
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | variable/Swift | computedGlobalVar
// MODULE-NEXT: 0:0 | variable(public)/Swift | computedGlobalVar
// MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// MODULE-NEXT: RelCont | variable/Swift | computedGlobalVar
// MODULE-NEXT: 0:0 | variable(public)/Swift | computedGlobalVar
public var computedGlobalVar: Int { 1 }

public struct Vars {
  @available(DynamicDomain)
  // CHECK: [[@LINE-1]]:14 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | static-property/Swift | staticVar1
  // CHECK: [[@LINE-3]]:14 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | static-property/Swift | staticVar2
  // MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // MODULE-NEXT: RelCont | static-property/Swift | staticVar1
  // MODULE-NEXT: 0:0 | static-property(public)/Swift | staticVar1
  // MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // MODULE-NEXT: RelCont | static-property/Swift | staticVar2
  // MODULE-NEXT: 0:0 | static-property(public)/Swift | staticVar2
  public static var staticVar1 = 1, staticVar2 = 2

  @available(DynamicDomain)
  // CHECK: [[@LINE-1]]:14 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | instance-property/Swift | computed
  // MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // MODULE-NEXT: RelCont | instance-property/Swift | computed
  // MODULE-NEXT: 0:0 | instance-property(public)/Swift | computed
  public var computed: Int { 1 }

  // Accessors are visited more than once, but are indexed only once.
  public var accessors: Int {
    @available(DynamicDomain)
    // CHECK: [[@LINE-1]]:16 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | instance-method/acc-get/Swift | getter:accessors
    // CHECK-NEXT: [[@LINE+6]]:5 | instance-method/acc-get/Swift | getter:accessors
    // MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
    // MODULE-NEXT: RelCont | instance-method/acc-get/Swift | getter:accessors
    // MODULE-NEXT: 0:0 | instance-method/acc-get/Swift | getter:accessors
    // MODULE-NEXT: RelChild,RelAcc | instance-property/Swift | accessors
    // MODULE-NEXT: 0:0 | variable/Swift | __clang_availability_domain_EnabledDomain
    get { 1 }
    @available(EnabledDomain)
    // CHECK: [[@LINE-1]]:16 | variable/Swift | __clang_availability_domain_EnabledDomain | c:availability_domains.h@__clang_availability_domain_EnabledDomain | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | instance-method/acc-set/Swift | setter:accessors
    // CHECK-NEXT: [[@LINE+5]]:5 | instance-method/acc-set/Swift | setter:accessors
    // MODULE-SAME: | c:availability_domains.h@__clang_availability_domain_EnabledDomain | Ref,RelCont | rel: 1
    // MODULE-NEXT: RelCont | instance-method/acc-set/Swift | setter:accessors
    // MODULE-NEXT: 0:0 | instance-method/acc-set/Swift | setter:accessors
    // CHECK-NOT: availability_domain
    set {}
  }

  public subscript(i: Int) -> Int {
    @available(DynamicDomain)
    // CHECK: [[@LINE-1]]:16 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | instance-method/acc-get/Swift | getter:subscript(_:)
    // CHECK-NEXT: [[@LINE+6]]:5 | instance-method/acc-get/Swift | getter:subscript(_:)
    // CHECK-NOT: availability_domain
    // MODULE: 0:0 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
    // MODULE-NEXT: RelCont | instance-method/acc-get/Swift | getter:subscript(_:)
    // MODULE-NEXT: 0:0 | instance-method/acc-get/Swift | getter:subscript(_:)
    // MODULE-NOT: availability_domain
    get { 1 }
  }
}

// Function bodies are not indexed in a binary module.
// MODULE: 0:0 | function(public)/Swift | queries()
// MODULE-NOT: availability_domain
public func queries() {
  if #available(DynamicDomain) {}
  // CHECK: [[@LINE-1]]:17 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | function/Swift | queries()

  if #unavailable(EnabledDomain) {}
  // CHECK: [[@LINE-1]]:19 | variable/Swift | __clang_availability_domain_EnabledDomain | c:availability_domains.h@__clang_availability_domain_EnabledDomain | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | function/Swift | queries()

  if #available(AvailMacro) {}
  // CHECK: [[@LINE-1]]:17 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | function/Swift | queries()

  guard #available(DynamicDomain) else { return }
  // CHECK: [[@LINE-1]]:20 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | function/Swift | queries()
}

//--- main.swift
import AvailabilityDomains

// Top-level code has no enclosing declaration to contain the references.

if #available(DynamicDomain) {}
// CHECK: [[@LINE-1]]:15 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref | rel: 0

if #available(AvailMacro) {}
// CHECK: [[@LINE-1]]:15 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref | rel: 0

@available(DynamicDomain)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | topLevelVar
var topLevelVar = 1

@available(AvailMacro)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | topLevelVar1
// CHECK: [[@LINE-3]]:12 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | topLevelVar2
var topLevelVar1 = 1, topLevelVar2 = 2

// Stored instance properties cannot have a custom availability domain, so
// this does not compile. Check that the attribute is still indexed once for
// each variable in the binding.
struct InvalidStoredProperties {
  @available(DynamicDomain)
  // CHECK: [[@LINE-1]]:14 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | instance-property/Swift | first
  // CHECK: [[@LINE-3]]:14 | variable/Swift | __clang_availability_domain_DynamicDomain | c:availability_domains.h@__clang_availability_domain_DynamicDomain | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | instance-property/Swift | second
  var first = 1, second = 2
}
