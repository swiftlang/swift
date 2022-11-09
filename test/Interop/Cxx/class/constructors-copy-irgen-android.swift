// Target-specific tests for C++ copy constructor code generation.

// RUN: %swift -module-name MySwift -target aarch64-unknown-linux-android -dump-clang-diagnostics -I %S/Inputs -enable-experimental-cxx-interop -emit-ir %s -parse-stdlib -parse-as-library -disable-legacy-type-info | %FileCheck %s -check-prefix=ITANIUM_ARM

// REQUIRES: OS=linux-android
// REQUIRES: CPU=aarch64

import Constructors
import TypeClassification

// ITANIUM_ARM-LABEL: define protected swiftcc void @"$s7MySwift35copyWithUserProvidedCopyConstructorySo03Has{{cdeF0V_ACtACF|efgH0V_ADtADF}}"
// ITANIUM_ARM-SAME: (%TSo30HasUserProvidedCopyConstructorV* {{.*}}[[ARG0:%.*]], %TSo30HasUserProvidedCopyConstructorV* {{.*}}[[ARG1:%.*]], %TSo30HasUserProvidedCopyConstructorV* {{.*}}[[ARG2:%.*]])
// ITANIUM_ARM: [[ARG0_AS_STRUCT:%.*]] = bitcast %TSo30HasUserProvidedCopyConstructorV* [[ARG0]] to %struct.HasUserProvidedCopyConstructor*
// ITANIUM_ARM: [[ARG2_AS_STRUCT:%.*]] = bitcast %TSo30HasUserProvidedCopyConstructorV* [[ARG2]] to %struct.HasUserProvidedCopyConstructor*
// ITANIUM_ARM: call void @_ZN30HasUserProvidedCopyConstructorC2ERKS_(%struct.HasUserProvidedCopyConstructor* [[ARG0_AS_STRUCT]], %struct.HasUserProvidedCopyConstructor* [[ARG2_AS_STRUCT]])
// ITANIUM_ARM: [[ARG1_AS_STRUCT:%.*]] = bitcast %TSo30HasUserProvidedCopyConstructorV* [[ARG1]] to %struct.HasUserProvidedCopyConstructor*
// ITANIUM_ARM: [[ARG2_AS_STRUCT:%.*]] = bitcast %TSo30HasUserProvidedCopyConstructorV* [[ARG2]] to %struct.HasUserProvidedCopyConstructor*
// ITANIUM_ARM: call void @_ZN30HasUserProvidedCopyConstructorC2ERKS_(%struct.HasUserProvidedCopyConstructor* [[ARG1_AS_STRUCT]], %struct.HasUserProvidedCopyConstructor* [[ARG2_AS_STRUCT]])
// ITANIUM_ARM: ret void

public func copyWithUserProvidedCopyConstructor(_ x: HasUserProvidedCopyConstructor)
  -> (HasUserProvidedCopyConstructor, HasUserProvidedCopyConstructor) {
  return (x, x)
}
