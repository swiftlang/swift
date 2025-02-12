// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test -swift-version 5 -print-module -module-to-print=IncludesCxxStdlib -I %t/Inputs -source-filename=test.swift -enable-experimental-cxx-interop -module-cache-path %t/cache | %FileCheck %s

//--- Inputs/module.modulemap
module IncludesCxxStdlib {
    header "header.h"
    export *
}

//--- Inputs/header.h
#include <vector>

class SimplePOD {
public:
    int x;
};

class __attribute__((swift_attr("import_reference"), swift_attr("retain:immortal"), swift_attr("release:immortal"))) FRTType {
public:
    int y;
};

// Type aliases are needed as a temporary workaround for
// https://github.com/apple/swift/issues/65446.
using T_1 = std::vector<SimplePOD> ;
using T_2 = std::vector<FRTType *> ;
using T_3 = std::vector<const SimplePOD *> ;
using T_4 = std::vector<SimplePOD *> ;

class VecOwner {
public:
    std::vector<SimplePOD> getPODItems() const;

    std::vector<FRTType *> getFRTItems() const;

    std::vector<const SimplePOD * _Nonnull> getPODPtrItems() const;

    std::vector<SimplePOD * _Nullable> getMutPODPtrItems() const;
};

// CHECK: func getPODItems() -> std{{\.__(ndk)?1\.|\.}}vector<SimplePOD, std{{\.__(ndk)?1\.|\.}}allocator<SimplePOD>>
// CHECK: func getFRTItems() -> std{{\.__(ndk)?1\.|\.}}vector<FRTType, std{{\.__(ndk)?1\.|\.}}allocator<FRTType>>
// CHECK: func getPODPtrItems() -> std{{\.__(ndk)?1\.|\.}}vector<UnsafePointer<SimplePOD>, std{{\.__(ndk)?1\.|\.}}allocator<UnsafePointer<SimplePOD>>>
// CHECK: func getMutPODPtrItems() -> std{{\.__(ndk)?1\.|\.}}vector<UnsafeMutablePointer<SimplePOD>, std{{\.__(ndk)?1\.|\.}}allocator<UnsafeMutablePointer<SimplePOD>>>
