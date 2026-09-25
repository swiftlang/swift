// REQUIRES: swift_feature_CustomAvailability

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %t/test.swift \
// RUN:   -I %t -enable-experimental-feature CustomAvailability \
// RUN:   -define-availability "_arctic:Arctic" \
// RUN:   | %FileCheck -dump-input=always %t/test.swift

//--- module.modulemap
module AvailabilityDomains {
  header "availability_domains.h"
  export *
}

//--- availability_domains.h
#include <availability_domain.h>

int arctic_pred(void);

CLANG_DYNAMIC_AVAILABILITY_DOMAIN(Arctic, arctic_pred);
CLANG_ENABLED_AVAILABILITY_DOMAIN(Pacific);

//--- test.swift
import AvailabilityDomains

// A domain that has no declaration behind it contributes nothing.

@available(macOS 99, *)
func platformOnly() {}
// CHECK: [[@LINE-1]]:6 | function(internal)/Swift | platformOnly()
// CHECK-NOT: availability_domain

// Attributes.

@available(Arctic)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_Arctic | c:availability_domains.h@__clang_availability_domain_Arctic | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | availableInArctic()
func availableInArctic() {}

@available(Pacific, unavailable)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_Pacific | c:availability_domains.h@__clang_availability_domain_Pacific | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | unavailableInPacific()
func unavailableInPacific() {}

// A custom domain cannot be spelled on a stored property, so this does not
// compile. The indexer still runs over code that does not compile, and the one
// attribute reaches it once per binding, so check that it reports the domain
// only once.

struct Bindings {
  @available(Arctic)
  // CHECK: [[@LINE-1]]:14 | variable/Swift | __clang_availability_domain_Arctic | c:availability_domains.h@__clang_availability_domain_Arctic | Ref | rel: 0
  // CHECK-NOT: [[@LINE-2]]:14 | variable/Swift | __clang_availability_domain_Arctic
  var first = 1, second = 2
}

// Queries.

func queries() {
  if #available(Arctic) {}
  // CHECK: [[@LINE-1]]:17 | variable/Swift | __clang_availability_domain_Arctic | c:availability_domains.h@__clang_availability_domain_Arctic | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | function/Swift | queries()

  if #unavailable(Pacific) {}
  // CHECK: [[@LINE-1]]:19 | variable/Swift | __clang_availability_domain_Pacific | c:availability_domains.h@__clang_availability_domain_Pacific | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | function/Swift | queries()

  guard #available(Arctic) else { return }
  // CHECK: [[@LINE-1]]:20 | variable/Swift | __clang_availability_domain_Arctic | c:availability_domains.h@__clang_availability_domain_Arctic | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | function/Swift | queries()
}

// An availability macro is indexed at the macro name, not at the
// -define-availability argument that spells the domain.

@available(_arctic)
// CHECK: [[@LINE-1]]:12 | variable/Swift | __clang_availability_domain_Arctic | c:availability_domains.h@__clang_availability_domain_Arctic | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | fromAttributeMacro()
func fromAttributeMacro() {}

func fromQueryMacro() {
  if #available(_arctic) {}
  // CHECK: [[@LINE-1]]:17 | variable/Swift | __clang_availability_domain_Arctic | c:availability_domains.h@__clang_availability_domain_Arctic | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | function/Swift | fromQueryMacro()
}

// Top level code has no enclosing declaration to act as the decl context.

if #available(Arctic) {}
// CHECK: [[@LINE-1]]:15 | variable/Swift | __clang_availability_domain_Arctic | c:availability_domains.h@__clang_availability_domain_Arctic | Ref | rel: 0
