// REQUIRES: OS=linux-gnu || OS=freebsd
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -function-sections -emit-module -emit-library -static -parse-stdlib %S/Inputs/FunctionSections.swift -o %t/libFunctionSections.a
// RUN: %target-build-swift -Xlinker --gc-sections -Xlinker -Map=%t/FunctionSections.map -I%t -L%t -lFunctionSections %S/Inputs/FunctionSectionsUse.swift
// RUN: if head -1 %t/FunctionSections.map | grep "Archive members"; then  %FileCheck --check-prefix GOLD %s < %t/FunctionSections.map ; fi
// RUN: if head -1 %t/FunctionSections.map | grep "VMA"; then %FileCheck --check-prefix LLD %s < %t/FunctionSections.map ; fi

// GOLD: Discarded input sections
// GOLD: .text.$s16FunctionSections5func2yyF
// GOLD: Memory map
// GOLD: .text.$s16FunctionSections5func1yyF

// LLD: .text.$s16FunctionSections5func1yyF
