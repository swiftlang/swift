#pragma once

#include "visibility.h"

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

// A wrapper around C++'s static_cast(), which allows Swift to get around interop's current lack of support for inheritance.
template <class I, class O> O cxxCast(I i) { return static_cast<O>(i); }

// A minimal foreign reference type.
struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
BaseT {
public:
    bool isBase;
    BaseT() { isBase = true; }
    BaseT(const BaseT &) = delete;
    static BaseT &getBaseT() { static BaseT singleton; return singleton; }
};

// A foreign reference type that is a subclass of BaseT.
struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
SubT : BaseT {
public:
    SubT() { isBase = false; }
    SubT(const SubT &) = delete;
    static SubT &getSubT() { static SubT singleton; return singleton; }
};

struct ValueType {};
ValueType *returnValueType() { return new ValueType(); };

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:RCRetain")))
__attribute__((swift_attr("release:RCRelease"))) RefType {};
void RCRetain(RefType *v) {}
void RCRelease(RefType *v) {}
RefType *returnRefType() { return new RefType(); };

struct DerivedFromRefType : RefType {};
DerivedFromRefType *returnDerivedFromRefType() {
  return new DerivedFromRefType();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:baseRetain1")))
__attribute__((swift_attr("release:baseRelease1"))) BaseRef1 {};
void baseRetain1(BaseRef1 *v) {}
void baseRelease1(BaseRef1 *v) {}
BaseRef1 *returnBaseRef1() { return new BaseRef1(); };

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:baseRetain2")))
__attribute__((swift_attr("release:baseRelease2"))) BaseRef2 {};
void baseRetain2(BaseRef2 *v) {}
void baseRelease2(BaseRef2 *v) {}
BaseRef2 *returnBaseRef2() { return new BaseRef2(); };

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:baseRetain1")))
__attribute__((swift_attr("release:baseRelease1"))) BaseRef3 : BaseRef1 {};
BaseRef3 *returnBaseRef3() { return new BaseRef3(); };

struct DerivedFromBaseRef1AndBaseRef2 : BaseRef1, BaseRef2 {};
DerivedFromBaseRef1AndBaseRef2 *returnDerivedFromBaseRef1AndBaseRef2() {
  return new DerivedFromBaseRef1AndBaseRef2();
};

struct DerivedFromBaseRef3 : BaseRef3 {};
DerivedFromBaseRef3 *returnDerivedFromBaseRef3() {
  return new DerivedFromBaseRef3();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:bRetain1")))
__attribute__((swift_attr("release:bRelease1"))) B1 {};
void bRetain1(B1 *v) {}
void bRelease1(B1 *v) {}
B1 *returnB1() { return new B1(); };

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:bRetain2")))
__attribute__((swift_attr("release:bRelease2"))) B2 {};
void bRetain2(B2 *v) {}
void bRelease2(B2 *v) {}
B2 *returnB2() { return new B2(); };

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:bRetain3")))
__attribute__((swift_attr("release:bRelease3"))) B3 {};
void bRetain3(B3 *v) {}
void bRelease3(B3 *v) {}
B3 *returnB3() { return new B3(); };

struct D : B1, B2, B3 {};
D *returnD() { return new D(); };

SWIFT_END_NULLABILITY_ANNOTATIONS
