// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The generated header calls the metadata accessors of imported types, so they
// are exported for C++ code outside of the Swift module's image.
// RUN: %target-swift-frontend -emit-ir %t/use-cxx-types.swift -module-name UseCxx -I %t \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-object-format \
// RUN:       --implicit-check-not='$sSo13ExtensionOnlyVMa'

// RUN: %target-swift-frontend -typecheck %t/use-cxx-types.swift -module-name UseCxx -I %t \
// RUN:   -cxx-interoperability-mode=default -clang-header-expose-decls=all-public \
// RUN:   -emit-clang-header-path %t/UseCxx.h
// RUN: %FileCheck %s --check-prefix=HEADER --implicit-check-not='$sSo2nsOMa' < %t/UseCxx.h

// Other object files of the module don't declare the accessor as hidden.
// RUN: %target-swift-frontend -emit-ir %t/use-cxx-types.swift %t/other.swift \
// RUN:   -module-name UseCxx -I %t -cxx-interoperability-mode=default -num-threads 2 \
// RUN:   -o %t/use-cxx-types.ll -o %t/other.ll
// RUN: cat %t/use-cxx-types.ll %t/other.ll | %FileCheck %s --check-prefix=MULTI

// Type metadata for foreign types is not yet supported on Windows. Bug #76168.
// UNSUPPORTED: OS=windows-msvc

//--- header.h

struct CStruct {
  int x;
};

struct CxxStruct {
  inline CxxStruct(int x) : x(x) {}
  inline CxxStruct(const CxxStruct &other) : x(other.x) {}
  inline ~CxxStruct() {}

  int x;
};

namespace ns {
struct Inner {
  int x;
};
} // namespace ns

typedef struct {
  int x;
} AnonTypedef;

enum class Color { red, green };

template <class T>
struct Tmpl {
  T x;
};
using TmplInt = Tmpl<int>;

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) ImmortalRef {
  int x;
};

struct ParamOnly {
  int x;
};

struct EnumPayload {
  int x;
};

struct InternalOnly {
  int x;
};

struct ExtensionOnly {
  int x;
};

//--- module.modulemap
module CxxTest {
  header "header.h"
  requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public func takeCStruct(_ x: CStruct?) {}
public func takeCxxStruct(_ x: CxxStruct?) {}
public func takeInner(_ x: [ns.Inner]) {}
public func takeAnonTypedef(_ x: [AnonTypedef]) {}
public func takeColor(_ x: [Color]) {}
public func takeTmpl(_ x: [TmplInt]) {}
@available(SwiftStdlib 5.8, *)
public func takeImmortalRef(_ x: [ImmortalRef]) {}

public func takeParamOnly(_ x: ParamOnly) {}

public enum WithPayload {
  case payload(EnumPayload)
}

public func makeAny() -> Any {
  return InternalOnly(x: 1)
}

// The header doesn't expose members of extensions of imported types.
extension CStruct {
  public func extensionOnly() -> ExtensionOnly {
    return ExtensionOnly(x: x)
  }
}

// CHECK-macho-DAG: define weak_odr swiftcc %swift.metadata_response @"$sSo7CStructVMa"(
// CHECK-elf-DAG: define weak_odr protected swiftcc %swift.metadata_response @"$sSo7CStructVMa"(
// CHECK-DAG: define weak_odr {{(protected )?}}swiftcc %swift.metadata_response @"$sSo9CxxStructVMa"(
// CHECK-DAG: define weak_odr {{(protected )?}}swiftcc %swift.metadata_response @"$sSo2nsO5InnerVMa"(
// CHECK-DAG: define weak_odr {{(protected )?}}swiftcc %swift.metadata_response @"$sSo11AnonTypedefaMa"(
// CHECK-DAG: define weak_odr {{(protected )?}}swiftcc %swift.metadata_response @"$sSo5ColorVMa"(
// CHECK-DAG: define weak_odr {{(protected )?}}swiftcc %swift.metadata_response @"$sSo0016TmplCInt_vAJBbyaVMa"(
// CHECK-DAG: define weak_odr {{(protected )?}}swiftcc %swift.metadata_response @"$sSo11ImmortalRefVMa"(
// CHECK-DAG: define weak_odr {{(protected )?}}swiftcc %swift.metadata_response @"$sSo9ParamOnlyVMa"(
// CHECK-DAG: define weak_odr {{(protected )?}}swiftcc %swift.metadata_response @"$sSo11EnumPayloadVMa"(

// Only ELF exports the accessors of types that the public API doesn't refer
// to.
// CHECK-macho-DAG: define linkonce_odr hidden swiftcc %swift.metadata_response @"$sSo12InternalOnlyVMa"(
// CHECK-elf-DAG: define weak_odr protected swiftcc %swift.metadata_response @"$sSo12InternalOnlyVMa"(

// The header doesn't refer to namespaces.
// CHECK-DAG: define linkonce_odr hidden swiftcc %swift.metadata_response @"$sSo2nsOMa"(

// HEADER: SWIFT_EXTERN swift::_impl::MetadataResponseTy $sSo11EnumPayloadVMa(
// HEADER: SWIFT_EXTERN swift::_impl::MetadataResponseTy $sSo11AnonTypedefaMa(
// HEADER: SWIFT_EXTERN swift::_impl::MetadataResponseTy $sSo7CStructVMa(
// HEADER: SWIFT_EXTERN swift::_impl::MetadataResponseTy $sSo5ColorVMa(
// HEADER: SWIFT_EXTERN swift::_impl::MetadataResponseTy $sSo9CxxStructVMa(
// HEADER: SWIFT_EXTERN swift::_impl::MetadataResponseTy $sSo11ImmortalRefVMa(
// HEADER: SWIFT_EXTERN swift::_impl::MetadataResponseTy $sSo2nsO5InnerVMa(
// HEADER: SWIFT_EXTERN swift::_impl::MetadataResponseTy $sSo9ParamOnlyVMa(
// HEADER: SWIFT_EXTERN swift::_impl::MetadataResponseTy $sSo0016TmplCInt_vAJBbyaVMa(

//--- other.swift
import CxxTest

public func box(_ x: CxxStruct) -> Any {
  return x
}

// MULTI-DAG: define weak_odr {{(protected )?}}swiftcc %swift.metadata_response @"$sSo9CxxStructVMa"(
// MULTI-DAG: declare swiftcc %swift.metadata_response @"$sSo9CxxStructVMa"(
