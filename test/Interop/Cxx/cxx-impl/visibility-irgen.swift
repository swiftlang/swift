// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The C++ entry point of a `@cxx @implementation` function is hidden if either
// the C++ declaration it implements is hidden or the Swift declaration is
// effectively less than public.

// RUN: %target-swift-emit-ir %t/main.swift -I %t \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -validate-tbd-against-ir=all \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,DEFAULT

// -enable-testing exports internal implementations, unless the C++ declaration
// is hidden.
// RUN: %target-swift-emit-ir %t/main.swift -I %t \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -validate-tbd-against-ir=all -enable-testing \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,TESTING

// -fvisibility=hidden makes unannotated C++ declarations hidden.
// RUN: %target-swift-emit-ir %t/main.swift -I %t \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -validate-tbd-against-ir=all -Xcc -fvisibility=hidden \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,FVIS-HIDDEN

// REQUIRES: swift_feature_CxxImplementation
// The checks use Itanium manglings.
// UNSUPPORTED: OS=windows-msvc

//--- module.modulemap
module Vis {
  header "Vis.h"
  requires cplusplus
}

//--- Vis.h
int defaultPublic();
int defaultInternal();
__attribute__((visibility("hidden"))) int hiddenPublic();

namespace __attribute__((visibility("hidden"))) HiddenNS {
int inHiddenNamespace();
}

struct __attribute__((visibility("hidden"))) HiddenStruct {
  int value;
  int get() const;
};

struct VisibleStruct {
  int value;
  int get() const;
  __attribute__((visibility("hidden"))) int hiddenGet() const;
};

struct HiddenRef;
void retainHiddenRef(HiddenRef *_Nonnull);
void releaseHiddenRef(HiddenRef *_Nonnull);

struct __attribute__((visibility("hidden")))
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainHiddenRef")))
__attribute__((swift_attr("release:releaseHiddenRef"))) HiddenRef {
  int value;
  virtual int describe() const;
};

//--- main.swift
import Vis

// DEFAULT-DAG: define{{( protected)?}} {{.*}}i32 @_Z13defaultPublicv()
// TESTING-DAG: define{{( protected)?}} {{.*}}i32 @_Z13defaultPublicv()
// FVIS-HIDDEN-DAG: define hidden {{.*}}i32 @_Z13defaultPublicv()
@cxx @implementation
public func defaultPublic() -> Int32 { 0 }

// DEFAULT-DAG: define hidden {{.*}}i32 @_Z15defaultInternalv()
// TESTING-DAG: define{{( protected)?}} {{.*}}i32 @_Z15defaultInternalv()
// FVIS-HIDDEN-DAG: define hidden {{.*}}i32 @_Z15defaultInternalv()
@cxx @implementation
func defaultInternal() -> Int32 { 0 }

// CHECK-DAG: define hidden {{.*}}i32 @_Z12hiddenPublicv()
@cxx @implementation
public func hiddenPublic() -> Int32 { 0 }

extension HiddenNS {
  // CHECK-DAG: define hidden {{.*}}i32 @_ZN8HiddenNS17inHiddenNamespaceEv()
  @cxx @implementation
  public static func inHiddenNamespace() -> Int32 { 0 }
}

extension HiddenStruct {
  // CHECK-DAG: define hidden {{.*}}i32 @_ZNK12HiddenStruct3getEv(
  @cxx @implementation
  public func get() -> Int32 { value }
}

extension VisibleStruct {
  // DEFAULT-DAG: define{{( protected)?}} {{.*}}i32 @_ZNK13VisibleStruct3getEv(
  // TESTING-DAG: define{{( protected)?}} {{.*}}i32 @_ZNK13VisibleStruct3getEv(
  // FVIS-HIDDEN-DAG: define hidden {{.*}}i32 @_ZNK13VisibleStruct3getEv(
  @cxx @implementation
  public func get() -> Int32 { value }

  // CHECK-DAG: define hidden {{.*}}i32 @_ZNK13VisibleStruct9hiddenGetEv(
  @cxx @implementation
  public func hiddenGet() -> Int32 { value }
}

extension HiddenRef {
  // CHECK-DAG: define hidden {{.*}}i32 @_ZNK9HiddenRef8describeEv(
  @cxx @implementation
  public func describe() -> Int32 { value }
}
