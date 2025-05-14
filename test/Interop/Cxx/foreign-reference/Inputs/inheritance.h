#pragma once

#include "visibility.h"

// A wrapper around C++'s static_cast(), which allows Swift to get around interop's current lack of support for inheritance.
template <class I, class O> O cxxCast(I i) { return static_cast<O>(i); }

namespace Foo {
template <class I, class O>
O cxxCast(I i) {
  return static_cast<O>(i);
}
} // namespace Foo

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

struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
BaseWithVirtualDestructor {
    int baseField = 123;

    BaseWithVirtualDestructor() {}
    virtual ~BaseWithVirtualDestructor() {}
};

struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
DerivedWithVirtualDestructor : public BaseWithVirtualDestructor {
    int derivedField = 456;

    DerivedWithVirtualDestructor() : BaseWithVirtualDestructor() {}
    ~DerivedWithVirtualDestructor() override {}
};

struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
DerivedOutOfOrder : public BaseT, public DerivedWithVirtualDestructor {
    // DerivedWithVirtualDestructor is the primary base class despite being the
    // second one the list.

    int leafField = 789; 

    DerivedOutOfOrder() = default;
    ~DerivedOutOfOrder() override {}

    static DerivedOutOfOrder& getInstance() {
      static DerivedOutOfOrder singleton;
      return singleton;
    }
};

struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
BaseAlign8 {
  long long field8 = 123;
}; // sizeof=8, dsize=8, align=8

struct DerivedHasTailPadding : public BaseAlign8 {
  int field4 = 456;
}; // sizeof=16, dsize=12, align=8

struct DerivedUsesBaseTailPadding : public DerivedHasTailPadding {
  short field2 = 789;

  static DerivedUsesBaseTailPadding& getInstance() {
    static DerivedUsesBaseTailPadding singleton;
    return singleton;
  }
}; // sizeof=16, dsize=14, align=8

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

namespace ImmortalRefereceExample {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) ImmortalRefType {};

ImmortalRefType *returnImmortalRefType() { return new ImmortalRefType(); };

struct DerivedFromImmortalRefType : ImmortalRefType {};
DerivedFromImmortalRefType *returnDerivedFromImmortalRefType() {
  return new DerivedFromImmortalRefType();
};

} // namespace ImmortalRefereceExample

namespace ExplicitAnnotationHasPrecedence1 {
struct ValueType {};
ValueType *returnValueType() { return new ValueType(); }

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:ret1")))
__attribute__((swift_attr("release:rel1"))) RefType {};
RefType *returnRefType() { return new RefType(); }

struct DerivedFromValueType : ValueType {};
DerivedFromValueType *returnDerivedFromValueType() {
    return new DerivedFromValueType();
}

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:ret2")))
__attribute__((swift_attr("release:rel2"))) DerivedFromValueTypeAndAnnotated
    : ValueType {};
DerivedFromValueTypeAndAnnotated *returnDerivedFromValueTypeAndAnnotated() {
  return new DerivedFromValueTypeAndAnnotated();
}

struct DerivedFromRefType final : RefType {};
DerivedFromRefType *returnDerivedFromRefType() {
  return new DerivedFromRefType();
}

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:ret3")))
__attribute__((swift_attr("release:rel3"))) DerivedFromRefTypeAndAnnotated
    : RefType {};
DerivedFromRefTypeAndAnnotated *returnDerivedFromRefTypeAndAnnotated() {
  return new DerivedFromRefTypeAndAnnotated();
}
} // namespace ExplicitAnnotationHasPrecedence1

void ret1(ExplicitAnnotationHasPrecedence1::RefType *v) {};
void rel1(ExplicitAnnotationHasPrecedence1::RefType *v) {};

void ret2(
    ExplicitAnnotationHasPrecedence1::DerivedFromValueTypeAndAnnotated *v) {};
void rel2(
    ExplicitAnnotationHasPrecedence1::DerivedFromValueTypeAndAnnotated *v) {};

void ret3(ExplicitAnnotationHasPrecedence1::DerivedFromRefTypeAndAnnotated *v) {
};
void rel3(ExplicitAnnotationHasPrecedence1::DerivedFromRefTypeAndAnnotated *v) {
};

namespace ExplicitAnnotationHasPrecedence2 {

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain_A")))
__attribute__((swift_attr("release:release_A"))) RefTypeA {};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain_B")))
__attribute__((swift_attr("release:release_B"))) RefTypeB {};

struct DerivedFromRefTypeAAndB : RefTypeA, RefTypeB {}; // expected-warning {{unable to infer SWIFT_SHARED_REFERENCE for 'DerivedFromRefTypeAAndB', although one of its transitive base types is marked as SWIFT_SHARED_REFERENCE}}
DerivedFromRefTypeAAndB *returnDerivedFromRefTypeAAndB() {
    return new DerivedFromRefTypeAAndB();
}

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain_C")))
__attribute__((swift_attr("release:release_C"))) DerivedFromRefTypeAAndBAnnotated
    : RefTypeA,
        RefTypeB {};
DerivedFromRefTypeAAndBAnnotated *returnDerivedFromRefTypeAAndBAnnotated() {
  return new DerivedFromRefTypeAAndBAnnotated();
}
} // namespace ExplicitAnnotationHasPrecedence2

void retain_A(ExplicitAnnotationHasPrecedence2::RefTypeA *v) {};
void release_A(ExplicitAnnotationHasPrecedence2::RefTypeA *v) {};
void retain_B(ExplicitAnnotationHasPrecedence2::RefTypeB *v) {};
void release_B(ExplicitAnnotationHasPrecedence2::RefTypeB *v) {};
void retain_C(
    ExplicitAnnotationHasPrecedence2::DerivedFromRefTypeAAndBAnnotated *v) {};
void release_C(
    ExplicitAnnotationHasPrecedence2::DerivedFromRefTypeAAndBAnnotated *v) {};

namespace BasicInheritanceExample {
struct ValueType {};
ValueType *returnValueType() { return new ValueType(); };

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:RCRetain")))
__attribute__((swift_attr("release:RCRelease"))) RefType {};

RefType *returnRefType() { return new RefType(); };

struct DerivedFromRefType final : RefType {};
DerivedFromRefType *returnDerivedFromRefType() {
  return new DerivedFromRefType();
};
} // namespace BasicInheritanceExample

void RCRetain(BasicInheritanceExample::RefType *v) {}
void RCRelease(BasicInheritanceExample::RefType *v) {}

namespace MultipleInheritanceExample1 {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:baseRetain1")))
__attribute__((swift_attr("release:baseRelease1"))) BaseRef1 {};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:baseRetain2")))
__attribute__((swift_attr("release:baseRelease2"))) BaseRef2 {};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:baseRetain1")))
__attribute__((swift_attr("release:baseRelease1"))) BaseRef3 : BaseRef1 {};

struct DerivedFromBaseRef1AndBaseRef2 : BaseRef1, BaseRef2 {}; // expected-warning {{unable to infer SWIFT_SHARED_REFERENCE for 'DerivedFromBaseRef1AndBaseRef2', although one of its transitive base types is marked as SWIFT_SHARED_REFERENCE}}
DerivedFromBaseRef1AndBaseRef2 *returnDerivedFromBaseRef1AndBaseRef2() {
  return new DerivedFromBaseRef1AndBaseRef2();
};

struct DerivedFromBaseRef3 : BaseRef3 {};
DerivedFromBaseRef3 *returnDerivedFromBaseRef3() {
  return new DerivedFromBaseRef3();
};
} // namespace MultipleInheritanceExample1

void baseRetain1(MultipleInheritanceExample1::BaseRef1 *v) {}
void baseRelease1(MultipleInheritanceExample1::BaseRef1 *v) {}

void baseRetain2(MultipleInheritanceExample1::BaseRef2 *v) {}
void baseRelease2(MultipleInheritanceExample1::BaseRef2 *v) {}

namespace MultipleInheritanceExample2 {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:bRetain1")))
__attribute__((swift_attr("release:bRelease1"))) B1 {};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:bRetain2")))
__attribute__((swift_attr("release:bRelease2"))) B2 {};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:bRetain3")))
__attribute__((swift_attr("release:bRelease3"))) B3 {};

struct D : B1, B2, B3 {}; // expected-warning {{unable to infer SWIFT_SHARED_REFERENCE for 'D', although one of its transitive base types is marked as SWIFT_SHARED_REFERENCE}}
D *returnD() { return new D(); };
} // namespace MultipleInheritanceExample2

void bRetain1(MultipleInheritanceExample2::B1 *v) {}
void bRelease1(MultipleInheritanceExample2::B1 *v) {}

void bRetain2(MultipleInheritanceExample2::B2 *v) {}
void bRelease2(MultipleInheritanceExample2::B2 *v) {}

void bRetain3(MultipleInheritanceExample2::B3 *v) {}
void bRelease3(MultipleInheritanceExample2::B3 *v) {}

// To check for x, y, x kind of pattern in parent's retain/release function name
// when infering SWIFT_SHARED_REFERENCE
namespace MultipleInheritanceExample3 {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain1")))
__attribute__((swift_attr("release:release1"))) B1 {};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain2")))
__attribute__((swift_attr("release:release2"))) B2 {};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain1")))
__attribute__((swift_attr("release:release1"))) B3 : B1 {};

struct D : B1, B2, B3 {}; // expected-warning {{unable to infer SWIFT_SHARED_REFERENCE for 'D', although one of its transitive base types is marked as SWIFT_SHARED_REFERENCE}}
D *returnD() { return new D(); };
} // namespace MultipleInheritanceExample3

void retain1(MultipleInheritanceExample3::B1 *v) {}
void release1(MultipleInheritanceExample3::B1 *v) {}

void retain2(MultipleInheritanceExample3::B2 *v) {}
void release2(MultipleInheritanceExample3::B2 *v) {}

namespace OverloadedRetainRelease {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:sameretain")))
__attribute__((swift_attr("release:samerelease"))) B1 {}; // expected-error {{multiple functions 'sameretain' found; there must be exactly one retain function for reference type 'B1'}}
                                                          // expected-error@-1 {{multiple functions 'samerelease' found; there must be exactly one release function for reference type 'B1'}}


struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:sameretain")))
__attribute__((swift_attr("release:samerelease"))) B2 {};  // expected-error {{multiple functions 'sameretain' found; there must be exactly one retain function for reference type 'B2'}}
                                                            // expected-error@-1 {{multiple functions 'samerelease' found; there must be exactly one release function for reference type 'B2'}}

struct D : B1, B2 {}; // expected-error {{multiple functions 'sameretain' found; there must be exactly one retain function for reference type 'D'}}
                      // expected-error@-1 {{multiple functions 'samerelease' found; there must be exactly one release function for reference type 'D'}}
D *returnD() { return new D(); };
} // namespace OverloadedRetainRelease

void sameretain(OverloadedRetainRelease::B1 *v) {}
void samerelease(OverloadedRetainRelease::B1 *v) {}

void sameretain(OverloadedRetainRelease::B2 *v) {}
void samerelease(OverloadedRetainRelease::B2 *v) {}

namespace RefTypeDiamondInheritance {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainA")))
__attribute__((swift_attr("release:releaseA"))) A {};


struct B : A {};

struct C : A {};

struct Diamond : B, C {}; // expected-warning {{unable to infer SWIFT_SHARED_REFERENCE for 'Diamond', although one of its transitive base types is marked as SWIFT_SHARED_REFERENCE}}
Diamond *returnDiamond() { return new Diamond(); };

struct BVirtual : virtual A {};

struct CVirtual : virtual A {};

struct VirtualDiamond : BVirtual, CVirtual {};
VirtualDiamond *returnVirtualDiamond() { return new VirtualDiamond(); };
} // namespace RefTypeDiamondInheritance

void retainA(RefTypeDiamondInheritance::A *a) {};
void releaseA(RefTypeDiamondInheritance::A *a) {};

namespace NonRefTypeDiamondInheritance {
struct A {};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainB")))
__attribute__((swift_attr("release:releaseB"))) B : A {};

struct C : A {};

struct Diamond : B, C {};
Diamond *returnDiamond() { return new Diamond(); };

} // namespace NonRefTypeDiamondInheritance

void retainB(NonRefTypeDiamondInheritance::B *a) {};
void releaseB(NonRefTypeDiamondInheritance::B *a) {};

namespace InheritingTemplatedRefType {

template <class T>
class __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:forestRetain"))) __attribute__((
    swift_attr("release:forestRelease"))) IntrusiveRefCountedTemplate {
public:
    IntrusiveRefCountedTemplate() : referenceCount(1) {}
    IntrusiveRefCountedTemplate(const IntrusiveRefCountedTemplate &) = delete;
    void retain() { ++referenceCount; }

    void release() {
    --referenceCount;
    if (referenceCount == 0)
        delete static_cast<T *>(this);
    }

private:
    int referenceCount;
};

class Forest : public IntrusiveRefCountedTemplate<Forest> {};
Forest *returnForest() { return new Forest(); };
} // namespace InheritingTemplatedRefType

void forestRetain(InheritingTemplatedRefType::IntrusiveRefCountedTemplate<
                    InheritingTemplatedRefType::Forest> *forest) {
    forest->retain();
}
void forestRelease(InheritingTemplatedRefType::IntrusiveRefCountedTemplate<
                    InheritingTemplatedRefType::Forest> *forest) {
    forest->release();
}  

SWIFT_END_NULLABILITY_ANNOTATIONS
