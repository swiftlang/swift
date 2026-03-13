// Target-specific tests for C++ copy constructor code generation.

// RUN: %swift -module-name MySwift -target aarch64-unknown-linux-android -dump-clang-diagnostics -I %S/Inputs -enable-experimental-cxx-interop -emit-ir %s -parse-stdlib -parse-as-library -disable-legacy-type-info -Xcc -fignore-exceptions | %FileCheck %s -check-prefix=ITANIUM_ARM

// REQUIRES: OS=linux-android
// REQUIRES: CPU=aarch64

import Constructors
import TypeClassification

// ITANIUM_ARM-LABEL: define protected swiftcc void @"$s7MySwift35copyWithUserProvidedCopyConstructorySo03Has{{cdeF0V_ACtACF|efgH0V_ADtADF}}"
// ITANIUM_ARM-SAME: (ptr {{.*}}[[ARG0:%.*]], ptr {{.*}}[[ARG1:%.*]], ptr {{.*}}[[ARG2:%.*]])
// ITANIUM_ARM: call void @_ZN30HasUserProvidedCopyConstructorC2ERKS_(ptr [[ARG0]], ptr [[ARG2]])
// ITANIUM_ARM: call void @_ZN30HasUserProvidedCopyConstructorC2ERKS_(ptr [[ARG1]], ptr [[ARG2]])
// ITANIUM_ARM: ret void

public func copyWithUserProvidedCopyConstructor(_ x: HasUserProvidedCopyConstructor)
  -> (HasUserProvidedCopyConstructor, HasUserProvidedCopyConstructor) {
  return (x, x)
}
