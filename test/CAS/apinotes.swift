// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t

// RUN: %FileCheck --input-file=%t/deps.json %s

// CHECK-NOT: -iapinotes-modules

//--- main.swift
import A

//--- module.modulemap
module A { header "A.h" export *}

//--- A.h
#pragma once
void a(void);

//--- A.apinotes
Name: A
Functions:
  - Name: a
    Availability: none
    AvailabilityMsg: "don't use this"
