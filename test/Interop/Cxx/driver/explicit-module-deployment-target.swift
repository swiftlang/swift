// REQUIRES: OS=macosx
// REQUIRES: sdk_min_version_15
// The legacy driver does not support explicit module builds.
// UNSUPPORTED: legacy_swift_driver

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: sed -e 's/TARGET_CPU/%target-cpu/g' %t/SDKLike.swiftinterface.in > %t/SDKLike.swiftinterface
// RUN: sed -e 's/TARGET_CPU/%target-cpu/g' %t/SDKLater.swiftinterface.in > %t/SDKLater.swiftinterface

// Fix the SDK Clang target so producer 15 and SDK 27 have different NewType
// definitions on every supported SDK. The client uses its deployment target.
// The client needs the SDK's newer declaration inside an availability check.
// Its inline C++ code must still use the client's deployment target.
// RUN: %target-swiftc_driver -emit-ir -Onone -explicit-module-build -experimental-clang-codegen-target-context -clang-target %target-cpu-apple-macosx27.0 -cxx-interoperability-mode=default -target %target-cpu-apple-macosx13.0 -module-cache-path %t/cache -I %t %t/client.swift -o %t/client.ll
// RUN: %FileCheck %s --check-prefix=OLDER --implicit-check-not=newerRuntime < %t/client.ll
// RUN: %FileCheck %s --check-prefix=OLDER-CONSTANT < %t/client.ll
// RUN: %FileCheck %s --check-prefix=SDK-API < %t/client.ll
// RUN: %FileCheck %s --check-prefix=UNOPTIMIZED < %t/client.ll
// RUN: %FileCheck %s --check-prefix=SUPPLEMENTAL < %t/client.ll
// RUN: %FileCheck %s --check-prefix=EXISTING < %t/client.ll

// The dependency scan must use the same optimization macros as the importer.
// RUN: %target-swiftc_driver -emit-ir -O -explicit-module-build -experimental-clang-codegen-target-context -clang-target %target-cpu-apple-macosx27.0 -cxx-interoperability-mode=default -target %target-cpu-apple-macosx13.0 -module-cache-path %t/cache -I %t %t/client.swift -o %t/optimized.ll
// RUN: %FileCheck %s --check-prefix=OPTIMIZED < %t/optimized.ll

// RUN: %target-swiftc_driver -emit-ir -Onone -explicit-module-build -experimental-clang-codegen-target-context -clang-target %target-cpu-apple-macosx27.0 -cxx-interoperability-mode=default -target %target-cpu-apple-macosx15.0 -module-cache-path %t/cache -I %t %t/client.swift -o %t/newer.ll
// RUN: %FileCheck %s --check-prefix=NEWER --implicit-check-not=olderRuntime < %t/newer.ll
// RUN: %FileCheck %s --check-prefix=NEWER-CONSTANT < %t/newer.ll

// Module emission must use the same definitions as source compilation. A
// separately compiled consumer must see the lower constant in an inlinable body.
// RUN: %target-swiftc_driver -emit-module -parse-as-library -module-name Contextual -emit-module-path %t/Contextual.swiftmodule -explicit-module-build -experimental-clang-codegen-target-context -clang-target %target-cpu-apple-macosx27.0 -cxx-interoperability-mode=default -target %target-cpu-apple-macosx13.0 -module-cache-path %t/cache -I %t %t/producer.swift
// RUN: %target-swiftc_driver -emit-ir -O -explicit-module-build -experimental-clang-codegen-target-context -clang-target %target-cpu-apple-macosx27.0 -cxx-interoperability-mode=default -target %target-cpu-apple-macosx13.0 -module-cache-path %t/cache -I %t %t/consumer.swift -o %t/consumer.ll
// RUN: %FileCheck %s --check-prefix=SERIALIZED < %t/consumer.ll

// A reference from an SDK module must not silently use an incompatible layout.
// RUN: not %target-swiftc_driver -typecheck -explicit-module-build -experimental-clang-codegen-target-context -clang-target %target-cpu-apple-macosx27.0 -cxx-interoperability-mode=default -target %target-cpu-apple-macosx13.0 -module-cache-path %t/cache -I %t %t/incompatible-layout.swift 2>&1 | %FileCheck %s --check-prefix=LAYOUT --implicit-check-not="Please submit a bug report"

// Ordinary member lookup retains producer 15, but an explicit reference from
// producer 27 must still reject its incompatible definition of the same type.
// RUN: not %target-swiftc_driver -typecheck -explicit-module-build -experimental-clang-codegen-target-context -clang-target %target-cpu-apple-macosx27.0 -cxx-interoperability-mode=default -target %target-cpu-apple-macosx13.0 -module-cache-path %t/cache -I %t %t/incompatible-producer.swift 2>&1 | %FileCheck %s --check-prefix=LAYOUT --implicit-check-not="Please submit a bug report" --implicit-check-not="Program aborted"

// OLDER: call {{.*}}@olderRuntime()
// NEWER: call {{.*}}@newerRuntime()

// A constant imported into Swift must use the same target as the inline body.
// OLDER-CONSTANT-LABEL: define {{.*}}readTargetConstant
// OLDER-CONSTANT: ret i32 13
// NEWER-CONSTANT-LABEL: define {{.*}}readTargetConstant
// NEWER-CONSTANT: ret i32 15
// SDK-API: call {{.*}}newAPI
// UNOPTIMIZED-LABEL: define {{.*}}readOptimizedConstant
// UNOPTIMIZED: ret i32 0
// OPTIMIZED-LABEL: define {{.*}}readOptimizedConstant
// OPTIMIZED: ret i32 1

// SERIALIZED-LABEL: define {{.*}}readSerializedConstant
// SERIALIZED: ret i32 13
// SERIALIZED-LABEL: define {{.*}}readSerializedOptimization
// SERIALIZED: ret i32 0

// LAYOUT: record definition differs between SDK and deployment targets

// SUPPLEMENTAL-DAG: call {{.*}}bounceCallback
// SUPPLEMENTAL-DAG: declare {{.*}}@_Z14chooseOverloadd
// SUPPLEMENTAL-DAG: declare {{.*}}@_ZNK14ExistingRecord9newMethodEv
// SUPPLEMENTAL-DAG: declare {{.*}}@_ZNK14ExistingRecord6selectEv
// SUPPLEMENTAL-DAG: declare {{.*}}@_ZNO14ExistingRecord9refSelectEv
// EXISTING-LABEL: define {{.*}}@_ZN14ExistingRecord6selectEv
// EXISTING: ret i32 21
// EXISTING-LABEL: define {{.*}}@_ZNR14ExistingRecord9refSelectEv
// EXISTING: ret i32 31

//--- module.modulemap
module VersionedHeader {
  requires cplusplus
  header "VersionedHeader.h"
  export *
}

//--- VersionedHeader.h
#pragma once

extern "C" int olderRuntime();
extern "C" int newerRuntime();

#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 150000
constexpr int targetConstant = 15;
#else
constexpr int targetConstant = 13;
#endif

#if defined(__OPTIMIZE__)
constexpr int optimizedConstant = 1;
#else
constexpr int optimizedConstant = 0;
#endif

// Linkage specifications are transparent to declaration lookup.
extern "C" {
inline int chooseRuntime() {
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 150000
  return newerRuntime();
#else
  return olderRuntime();
#endif
}

struct LayoutDependent {
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 150000
  long long padding;
#endif
  int value;
};
}

#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 150000
struct __attribute__((availability(macos, introduced = 15.0))) NewType {
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 270000
  long long value;
#else
  int value;
#endif
};
#endif

inline int chooseOverload(int value) { return chooseRuntime() + value; }

extern "C" {
struct ExistingRecord {
  int value;
  int select() __attribute__((swift_name("oldMutable()"))) { return 21; }
  int refSelect() & __attribute__((swift_name("oldLvalue()"))) { return 31; }
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 150000
  int newMethod() const __attribute__((availability(macos, introduced=15.0)));
  int select() const __attribute__((availability(macos, introduced=15.0)))
    __attribute__((swift_name("newConst()")));
  int refSelect() && __attribute__((availability(macos, introduced=15.0)))
    __attribute__((swift_name("newRvalue()")));
#endif
};
}

#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 150000
double chooseOverload(double value)
  __attribute__((availability(macos, introduced=15.0)));
struct RawOpaque;
using RawCallback = void (*)(RawOpaque *);
#endif

//--- SDKLike.swiftinterface.in
// swift-interface-format-version: 1.0
// swift-module-flags: -target TARGET_CPU-apple-macosx15.0 -enable-library-evolution -module-name SDKLike -formal-cxx-interoperability-mode=swift-6
import Swift
import VersionedHeader
@available(macOS 15.0, *)
public typealias SDKNewType = VersionedHeader.NewType
@available(macOS 15.0, *)
public func newAPI() -> VersionedHeader.NewType
@available(macOS 15.0, *)
public func incompatibleLayout() -> VersionedHeader.LayoutDependent
@available(macOS 15.0, *)
public func bounceCallback(_ callback: VersionedHeader.RawCallback) -> VersionedHeader.RawCallback

//--- SDKLater.swiftinterface.in
// swift-interface-format-version: 1.0
// swift-module-flags: -target TARGET_CPU-apple-macosx27.0 -enable-library-evolution -module-name SDKLater -formal-cxx-interoperability-mode=swift-6
import Swift
import VersionedHeader
@available(macOS 27.0, *)
public func laterAPI() -> VersionedHeader.NewType

//--- client.swift
import SDKLike
import VersionedHeader

public func choose() -> Int32 {
  chooseRuntime()
}

public func readTargetConstant() -> Int32 {
  targetConstant
}

public func readOptimizedConstant() -> Int32 {
  optimizedConstant
}

public func useNewAPI() -> Int32 {
  if #available(macOS 15.0, *) {
    return newAPI().value
  }
  return 0
}

// Deserializing this callback references an incomplete C++ record that is
// absent from the deployment-target header.
public func useCallback() {
  if #available(macOS 15.0, *) {
    _ = bounceCallback { _ in }
  }
}

// The new declarations must retain module ownership for Swiftify and lookup.
// Existing overloads keep their bodies and cv/ref qualifiers.
public func useExistingOverloads(_ value: Int32) -> Int32 {
  var record = ExistingRecord()
  return chooseOverload(value) + record.oldMutable() + record.oldLvalue()
}

public func useNewOverloads(_ value: Double) -> Double {
  if #available(macOS 15.0, *) {
    var record = ExistingRecord()
    return chooseOverload(value) + Double(record.newMethod())
      + Double(record.newConst()) + Double(record.newRvalue())
  }
  return 0
}

//--- producer.swift
import SDKLike
import VersionedHeader

@inlinable
public func serializedConstant() -> Int32 {
  targetConstant
}

@inlinable
public func serializedOptimization() -> Int32 {
  optimizedConstant
}

public func readLocalLayout(_ value: LayoutDependent) -> Int32 {
  value.value
}

// The module targets 13, but this reference retains the SDK interface's target
// of 15. A later consumer must not resolve it using the latest SDK's definition.
@available(macOS 15.0, *)
public func forwardedNewAPI() -> SDKLike.SDKNewType {
  newAPI()
}

//--- consumer.swift
import Contextual
import VersionedHeader

public func readSerializedConstant() -> Int32 {
  serializedConstant()
}

public func readSerializedOptimization() -> Int32 {
  serializedOptimization()
}

// The producer and consumer both use the lower Clang target. Their shared
// record remains valid even though the SDK has a different definition.
public func readSerializedLayout(_ value: LayoutDependent) -> Int32 {
  readLocalLayout(value)
}

public func readForwardedNewAPI() -> Int32 {
  if #available(macOS 15.0, *) {
    return forwardedNewAPI().value
  }
  return 0
}

//--- incompatible-layout.swift
import SDKLike
import VersionedHeader

public func readSDKLayout() -> Int32 {
  if #available(macOS 15.0, *) {
    return incompatibleLayout().value
  }
  return 0
}

//--- incompatible-producer.swift
import SDKLike
import SDKLater

public func readFirstProducer() -> Int32 {
  if #available(macOS 15.0, *) {
    return newAPI().value
  }
  return 0
}

public func readLaterProducer() -> Int64 {
  if #available(macOS 27.0, *) {
    return laterAPI().value
  }
  return 0
}
