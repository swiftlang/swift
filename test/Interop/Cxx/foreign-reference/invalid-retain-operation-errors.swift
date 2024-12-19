// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs %t/test.swift -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h
struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:nonexistent")))
    __attribute__((swift_attr("release:nonexistent")))
NonExistent {};

struct
        __attribute__((swift_attr("import_reference")))
NoRetainRelease {};

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetain")))
    __attribute__((swift_attr("release:badRelease")))
BadRetainRelease {};

int badRetain(BadRetainRelease *v);
void badRelease(BadRetainRelease *v, int i);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetainWithNullabilityAnnotations")))
    __attribute__((swift_attr("release:badReleaseWithNullabilityAnnotations")))
BadRetainReleaseWithNullabilityAnnotations {};

void badRetainWithNullabilityAnnotations(BadRetainRelease * _Nonnull v);
void badReleaseWithNullabilityAnnotations(BadRetainRelease * _Nonnull v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:goodRetain")))
    __attribute__((swift_attr("release:goodRelease")))
GoodRetainRelease {};

void goodRetain(GoodRetainRelease *v);
void goodRelease(GoodRetainRelease *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:goodRetainWithRetainReturningSelf")))
    __attribute__((swift_attr("release:goodReleaseWithRetainReturningSelf")))
GoodRetainReleaseWithRetainReturningSelf {};

GoodRetainReleaseWithRetainReturningSelf *goodRetainWithRetainReturningSelf(GoodRetainReleaseWithRetainReturningSelf *v);
void goodReleaseWithRetainReturningSelf(GoodRetainReleaseWithRetainReturningSelf *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:goodRetainWithNullabilityAnnotations")))
    __attribute__((swift_attr("release:goodReleaseWithNullabilityAnnotations")))
GoodRetainReleaseWithNullabilityAnnotations {};

void goodRetainWithNullabilityAnnotations(GoodRetainReleaseWithNullabilityAnnotations * _Nonnull v);
void goodReleaseWithNullabilityAnnotations(GoodRetainReleaseWithNullabilityAnnotations * _Nonnull v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetain2")))
    __attribute__((swift_attr("release:badRelease2")))
BadRetainRelease2 {};

void badRetain2(int);
void badRelease2(int);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetain3")))
    __attribute__((swift_attr("release:badRelease3")))
BadRetainRelease3 {};

void badRetain3(BadRetainRelease2 *);
void badRelease3(BadRetainRelease2 *);

struct nonCXXFRT{};

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetain4")))
    __attribute__((swift_attr("release:badRelease4")))
BadRetainRelease4 {};

void badRetain4(nonCXXFRT *);
void badRelease4(nonCXXFRT *);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:baseFRTRetain")))
    __attribute__((swift_attr("release:baseFRTRelease")))
BaseFRT {};

void baseFRTRetain(BaseFRT *v);
void baseFRTRelease(BaseFRT *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:derivedFRTRetain")))
    __attribute__((swift_attr("release:derivedFRTRelease")))
DerivedFRT : BaseFRT {};

void derivedFRTRetain(BaseFRT *v);
void derivedFRTRelease(BaseFRT *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:RCRetain")))
    __attribute__((swift_attr("release:RCRelease")))
RefCountedBase {};

void RCRetain(RefCountedBase *v);
void RCRelease(RefCountedBase *v);

struct 
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:RCRetain")))
    __attribute__((swift_attr("release:RCRelease")))
RefCountedDerived : RefCountedBase {};

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:base1FRTRetain")))
    __attribute__((swift_attr("release:base1FRTRelease")))
Base1FRT {};

void base1FRTRetain(Base1FRT *v);
void base1FRTRelease(Base1FRT *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:base2FRTRetain")))
    __attribute__((swift_attr("release:base2FRTRelease")))
Base2FRT : Base1FRT {};

void base2FRTRetain(Base1FRT *v);
void base2FRTRelease(Base1FRT *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:base3FRTRetain")))
    __attribute__((swift_attr("release:base3FRTRelease")))
Base3FRT : Base2FRT {};

void base3FRTRetain(Base1FRT *v);
void base3FRTRelease(Base1FRT *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:base4FRTRetain")))
    __attribute__((swift_attr("release:base4FRTRelease")))
Base4FRT : Base2FRT {};

void base4FRTRetain(GoodRetainRelease *v);
void base4FRTRelease(Base1FRT *v);

typedef struct 
__attribute__((swift_attr("import_reference"))) 
__attribute__((swift_attr("retain:anonymousStructRetain"))) 
__attribute__((swift_attr("release:anonymousStructRelease"))) 
{} AnonymousStruct;

void anonymousStructRetain(AnonymousStruct *v);
void anonymousStructRelease(AnonymousStruct *v);

typedef struct 
__attribute__((swift_attr("import_reference"))) 
__attribute__((swift_attr("retain:badAnonymousStructRetain"))) 
__attribute__((swift_attr("release:badAnonymousStructRelease"))) 
{} BadAnonymousStruct;

void badAnonymousStructRetain(AnonymousStruct *v);
void badAnonymousStructRelease(AnonymousStruct *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badFRTRetain")))
    __attribute__((swift_attr("release:badFRTRelease")))
BadFRT {};

int badFRTRetain = 0;
int badFRTRelease = 0;

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:retain")))
    __attribute__((swift_attr("release:release")))
MultipleRetainReleaseFRT {};

void retain(MultipleRetainReleaseFRT *v);
void retain(GoodRetainRelease *v);
void release(MultipleRetainReleaseFRT *v);
void release(GoodRetainRelease *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:retain2")))
    __attribute__((swift_attr("retain:retain1")))
    __attribute__((swift_attr("release:release1")))
    __attribute__((swift_attr("release:release2")))
MultipleRetainReleaseAttrFRT {};

void retain1(MultipleRetainReleaseAttrFRT *v);
void retain2(MultipleRetainReleaseAttrFRT *v);
void release1(MultipleRetainReleaseAttrFRT *v);
void release2(MultipleRetainReleaseAttrFRT *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:Uretain")))
    __attribute__((swift_attr("release:Urelease")))
UnimportedRetainRelease {};
void Uretain(UnimportedRetainRelease v);
UnimportedRetainRelease Urelease(UnimportedRetainRelease* v);

//--- test.swift

import Test

// CHECK: error: cannot find retain function 'nonexistent' for reference type 'NonExistent'
// CHECK: error: cannot find release function 'nonexistent' for reference type 'NonExistent'
@available(macOS 13.3, *)
public func test(x: NonExistent) { }

// CHECK: error: reference type 'NoRetainRelease' must have 'retain:' Swift attribute
// CHECK: error: reference type 'NoRetainRelease' must have 'release:' Swift attribute
@available(macOS 13.3, *)
public func test(x: NoRetainRelease) { }

// CHECK: error: specified retain function 'badRetain' is invalid; retain function must have 'void' or parameter return type
// CHECK: error: specified release function 'badRelease' is invalid; release function must have exactly one argument of type 'BadRetainRelease'
@available(macOS 13.3, *)
public func test(x: BadRetainRelease) { }

// CHECK: error: specified retain function 'badRetainWithNullabilityAnnotations' is invalid; retain function must have exactly one argument of type 'BadRetainReleaseWithNullabilityAnnotations'
// CHECK: error: specified release function 'badReleaseWithNullabilityAnnotations' is invalid; release function must have exactly one argument of type 'BadRetainReleaseWithNullabilityAnnotations'
@available(macOS 13.3, *)
public func test(x: BadRetainReleaseWithNullabilityAnnotations) { }

@available(macOS 13.3, *)
public func test(x: GoodRetainRelease) { }

@available(macOS 13.3, *)
public func test(x: GoodRetainReleaseRetainReturningSelf) { }

@available(macOS 13.3, *)
public func test(x: GoodRetainReleaseWithNullabilityAnnotations) { }

// CHECK: error: specified retain function 'badRetain2' is invalid; retain function must have exactly one argument of type 'BadRetainRelease2'
// CHECK: error: specified release function 'badRelease2' is invalid; release function must have exactly one argument of type 'BadRetainRelease2'
@available(macOS 13.3, *)
public func test(x: BadRetainRelease2) { }

// CHECK: error: specified retain function 'badRetain3' is invalid; retain function must have exactly one argument of type 'BadRetainRelease3'
// CHECK: error: specified release function 'badRelease3' is invalid; release function must have exactly one argument of type 'BadRetainRelease3'
@available(macOS 13.3, *)
public func test(x: BadRetainRelease3) { }

// CHECK: error: specified retain function 'badRetain4' is invalid; retain function must have exactly one argument of type 'BadRetainRelease4'
// CHECK: error: specified release function 'badRelease4' is invalid; release function must have exactly one argument of type 'BadRetainRelease4'
@available(macOS 13.3, *)
public func test(x: BadRetainRelease4) { }

@available(macOS 13.3, *)
public func test(x: DerivedFRT) { }

@available(macOS 13.3, *)
public func test(x: RefCountedDerived) { }

@available(macOS 13.3, *)
public func test(x: Base3FRT) { }

@available(macOS 13.3, *)
// CHECK: error: specified retain function 'base4FRTRetain' is invalid; retain function must have exactly one argument of type 'Base4FRT'
public func test(x: Base4FRT) { }

@available(macOS 13.3, *)
public func testAnonymousStruct(x: AnonymousStruct) { }

// CHECK: error: specified retain function 'badAnonymousStructRetain' is invalid; retain function must have exactly one argument of type 'BadAnonymousStruct'
// CHECK: error: specified release function 'badAnonymousStructRelease' is invalid; release function must have exactly one argument of type 'BadAnonymousStruct'
@available(macOS 13.3, *)
public func testBadAnonymousStruct(x: BadAnonymousStruct) { }

// CHECK: error: specified retain function 'badFRTRetain' is not a function
// CHECK: error: specified release function 'badFRTRelease' is not a function
@available(macOS 13.3, *)
public func testRetainReleaseAsNonFunction(x: BadFRT) {}

// CHECK: error: multiple functions 'retain' found; there must be exactly one retain function for reference type 'MultipleRetainReleaseFRT'
// CHECK: error: multiple functions 'release' found; there must be exactly one release function for reference type 'MultipleRetainReleaseFRT'
@available(macOS 13.3, *)
public func testMultipleRetainRelease(x: MultipleRetainReleaseFRT) {}

// CHECK: error: reference type 'MultipleRetainReleaseAttrFRT' must have only one 'retain:' Swift attribute
// CHECK: error: reference type 'MultipleRetainReleaseAttrFRT' must have only one 'release:' Swift attribute
@available(macOS 13.3, *)
public func testMultipleRetainRelease(x: MultipleRetainReleaseAttrFRT) {}

// CHECK: error: cannot find retain function 'Uretain' for reference type 'UnimportedRetainRelease'
// CHECK: note: function uses foreign reference type 'UnimportedRetainRelease' as a value in a parameter types which breaks 'swift_shared_reference' contract
// CHECK: error: cannot find release function 'Urelease' for reference type 'UnimportedRetainRelease'
// CHECK: note: function uses foreign reference type 'UnimportedRetainRelease' as a value in the return types which breaks 'swift_shared_reference' contract
@available(macOS 13.3, *)
public func test(x: UnimportedRetainRelease) {}
