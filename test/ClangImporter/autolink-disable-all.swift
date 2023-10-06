// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -sdk %S/Inputs -Fsystem %S/Inputs/System/Library/Frameworks -enable-objc-interop -I %S/Inputs/custom-modules -disable-all-autolinking -module-name AutolinkDisableFrameworks -emit-ir -o %t/test.ll
// RUN: cat %t/test.ll | %FileCheck %s

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: autolink-extract

import LinkMusket
import LinkFramework
import ClangModuleUser
import IndirectFrameworkImporter
import UsesSubmodule

// No linker options produced
// CHECK: !llvm.linker.options = !{}
