#pragma once

#include "visibility.h"

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

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

struct ValueType {};
ValueType *returnValueType() { return new ValueType(); };

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:RCRetain")))
__attribute__((swift_attr("release:RCRelease"))) RefType {};
void RCRetain(RefType *v) {}
void RCRelease(RefType *v) {}
RefType *returnRefType() {
  return new RefType();
};

struct DerivedFromRefType : RefType {};
DerivedFromRefType *returnDerivedFromRefType() {
  return new DerivedFromRefType();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:baseRetain1")))
__attribute__((swift_attr("release:baseRelease1"))) BaseRef1 {};
void baseRetain1(BaseRef1 *v) {}
void baseRelease1(BaseRef1 *v) {}
BaseRef1 *returnBaseRef1() { // expected-warning {{'returnBaseRef1' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE}}
  return new BaseRef1();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:baseRetain2")))
__attribute__((swift_attr("release:baseRelease2"))) BaseRef2 {};
void baseRetain2(BaseRef2 *v) {}
void baseRelease2(BaseRef2 *v) {}
BaseRef2 *returnBaseRef2() { // expected-warning {{'returnBaseRef2' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE}}
  return new BaseRef2();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:baseRetain1")))
__attribute__((swift_attr("release:baseRelease1"))) BaseRef3 : BaseRef1 {};
BaseRef3 *returnBaseRef3() {
  return new BaseRef3();
};

struct DerivedFromBaseRef1AndBaseRef2 : BaseRef1, // expected-warning {{unable to infer 'SWIFT_SHARED_REFERENCE' for 'DerivedFromBaseRef1AndBaseRef2', although one of its C++ inheritance parents is marked as 'SWIFT_SHARED_REFERENCE'}}
                                        BaseRef2 {
};
DerivedFromBaseRef1AndBaseRef2 *returnDerivedFromBaseRef1AndBaseRef2() {
  return new DerivedFromBaseRef1AndBaseRef2();
};

struct DerivedFromBaseRef3 : BaseRef3 {};
DerivedFromBaseRef3 *
returnDerivedFromBaseRef3() { // DOUBT: where is the warning??
  return new DerivedFromBaseRef3();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:bRetain1")))
__attribute__((swift_attr("release:bRelease1"))) B1 {};
void bRetain1(B1 *v) {}
void bRelease1(B1 *v) {}
B1 *returnB1() { // expected-warning {{'returnB1' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE}}
  return new B1();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:bRetain2")))
__attribute__((swift_attr("release:bRelease2"))) B2 {};
void bRetain2(B2 *v) {}
void bRelease2(B2 *v) {}
B2 *returnB2() { // expected-warning {{'returnB2' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE}}
  return new B2();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:bRetain3")))
__attribute__((swift_attr("release:bRelease3"))) B3 {};
void bRetain3(B3 *v) {}
void bRelease3(B3 *v) {}
B3 *returnB3() { // expected-warning {{'returnB3' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE}}
  return new B3();
};

struct D : B1, // expected-warning {{unable to infer 'SWIFT_SHARED_REFERENCE' for 'D', although one of its C++ inheritance parents is marked as 'SWIFT_SHARED_REFERENCE'}}
           B2,
           B3 {};
D *returnD() { return new D(); };

// To check for x, y, x kind of pattern in parent's retain/release function name
// when infering SWIFT_SHARED_REFERENCE
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain1")))
__attribute__((swift_attr("release:release1"))) B1_ {};
void retain1(B1_ *v) {}
void release1(B1_ *v) {}
B1_ *returnB1_() { // expected-warning {{'returnB1_' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE}}
  return new B1_();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain2")))
__attribute__((swift_attr("release:release2"))) B2_ {};
void retain2(B2_ *v) {}
void release2(B2_ *v) {}
B2_ *returnB2_() { // expected-warning {{'returnB2_' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE}}
  return new B2_();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain1")))
__attribute__((swift_attr("release:release1"))) B3_ : B1_ {};
B3_ *returnB3_() { // expected-warning {{'returnB3_' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE}}
  return new B3_();
};

struct D_ : B1_, // expected-warning {{direct base 'B1_' is inaccessible due to ambiguity:}}
                 // expected-warning@-1 {{unable to infer 'SWIFT_SHARED_REFERENCE' for 'D_', although one of its C++ inheritance parents is marked as 'SWIFT_SHARED_REFERENCE'}}
            B2_,
            B3_ {
};
D_ *returnD_() { return new D_(); };

// To check for overloaded retain/release functions
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:sameretain")))
__attribute__((swift_attr("release:samerelease"))) B1__ { // expected-error {{multiple functions 'sameretain' found; there must be exactly one retain function for reference type 'B1__'}} 
                                                          // expected-error@-1 {{multiple functions 'samerelease' found; there must be exactly one release function for reference type 'B1__'}}
}; 
void sameretain(B1__ *v) {}
void samerelease(B1__ *v) {}
B1__ *returnB1__() { // expected-warning {{'returnB1__' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE}}
  return new B1__();
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:sameretain")))
__attribute__((swift_attr("release:samerelease"))) B2__ { // expected-error {{multiple functions 'sameretain' found; there must be exactly one retain function for reference type 'B2__'}} 
                                                          // expected-error@-1 {{multiple functions 'samerelease' found; there must be exactly one release function for reference type 'B2__'}}
};
void sameretain(B2__ *v) {}
void samerelease(B2__ *v) {}
B2__ *returnB2__() { // expected-warning {{'returnB2__' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE}}
  return new B2__();
};

struct D__ : B1__, // expected-warning {{unable to infer 'SWIFT_SHARED_REFERENCE' for 'D__', although one of its C++ inheritance parents is marked as 'SWIFT_SHARED_REFERENCE'}}
             B2__ {
};
D__ *returnD__() { return new D__(); };

// To check for diamond inheritacne
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainA")))
__attribute__((swift_attr("release:releaseA"))) A {};
void retainA(A *a) {};
void releaseA(A *a) {};
A *returnA() { return new A(); };

struct B : A {};
B *returnB() { return new B(); };

struct C : A {};
C *returnC() { return new C(); };

struct Diamond : B, C {}; // expected-warning {{unable to infer 'SWIFT_SHARED_REFERENCE' for 'Diamond', although one of its C++ inheritance parents is marked as 'SWIFT_SHARED_REFERENCE'}}
Diamond *returnDiamond() { return new Diamond(); };

struct BVirtual : virtual A {};
BVirtual *returnBVirtual() { return new BVirtual(); };

struct CVirtual : virtual A {};
CVirtual *returnCVirtual() { return new CVirtual(); };

struct VirtualDiamond : BVirtual, CVirtual {};
VirtualDiamond *returnVirtualDiamond() { return new VirtualDiamond(); };

template <class T>
class __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain")))
__attribute__((swift_attr("release:release"))) IntrusiveRefCountedTemplate {
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

void retain(IntrusiveRefCountedTemplate<Forest> *forest) { forest->retain(); }
void release(IntrusiveRefCountedTemplate<Forest> *forest) { forest->release(); }

SWIFT_END_NULLABILITY_ANNOTATIONS
