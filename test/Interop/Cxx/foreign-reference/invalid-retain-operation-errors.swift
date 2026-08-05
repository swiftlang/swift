// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes \
// RUN:   -cxx-interoperability-mode=default -disable-availability-checking \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}test.h \
// RUN:   %t%{fs-sep}test.swift

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h
#pragma once
#pragma clang assume_nonnull begin

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:nonexistent")))
    __attribute__((swift_attr("release:nonexistent")))
NonExistent {};
// expected-error@-1 {{cannot find retain function 'nonexistent' for reference type 'NonExistent'}}
// expected-error@-2 {{cannot find release function 'nonexistent' for reference type 'NonExistent'}}

struct
        __attribute__((swift_attr("import_reference")))
NoRetainRelease {};
// expected-error@-1 {{reference type 'NoRetainRelease' must have exactly one 'retain:' Swift attribute}}
// expected-error@-2 {{reference type 'NoRetainRelease' must have exactly one 'release:' Swift attribute}}

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetain")))
    __attribute__((swift_attr("release:badRelease")))
BadRetainRelease {};
// expected-error@-1 {{specified retain function 'badRetain' is invalid; retain function must either return have 'void', the reference count as an integer, or the parameter type}}
// expected-error@-2 {{specified release function 'badRelease' is invalid; release function must have exactly one argument of type 'BadRetainRelease'}}

float badRetain(BadRetainRelease *v);
void badRelease(BadRetainRelease *v, int i);

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

void goodRetainWithNullabilityAnnotations(GoodRetainReleaseWithNullabilityAnnotations * _Nullable v);
void goodReleaseWithNullabilityAnnotations(GoodRetainReleaseWithNullabilityAnnotations * _Null_unspecified v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetain2")))
    __attribute__((swift_attr("release:badRelease2")))
BadRetainRelease2 {};
// expected-error@-1 {{specified retain function 'badRetain2' is invalid; retain function must have exactly one argument of type 'BadRetainRelease2'}}
// expected-error@-2 {{specified release function 'badRelease2' is invalid; release function must have exactly one argument of type 'BadRetainRelease2'}}

void badRetain2(int);
void badRelease2(int);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetain3")))
    __attribute__((swift_attr("release:badRelease3")))
BadRetainRelease3 {};
// expected-error@-1 {{specified retain function 'badRetain3' is invalid; retain function must have exactly one argument of type 'BadRetainRelease3'}}
// expected-error@-2 {{specified release function 'badRelease3' is invalid; release function must have exactly one argument of type 'BadRetainRelease3'}}

void badRetain3(BadRetainRelease2 *);
void badRelease3(BadRetainRelease2 *);

struct nonCXXFRT{};

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetain4")))
    __attribute__((swift_attr("release:badRelease4")))
BadRetainRelease4 {};
// expected-error@-1 {{specified retain function 'badRetain4' is invalid; retain function must have exactly one argument of type 'BadRetainRelease4'}}
// expected-error@-2 {{specified release function 'badRelease4' is invalid; release function must have exactly one argument of type 'BadRetainRelease4'}}

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
// expected-error@-1 {{specified retain function 'base4FRTRetain' is invalid; retain function must have exactly one argument of type 'Base4FRT'}}

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
// expected-error@-5{{specified retain function 'badAnonymousStructRetain' is invalid; retain function must have exactly one argument of type 'BadAnonymousStruct'}}
// expected-error@-6{{specified release function 'badAnonymousStructRelease' is invalid; release function must have exactly one argument of type 'BadAnonymousStruct'}}

void badAnonymousStructRetain(AnonymousStruct *v);
void badAnonymousStructRelease(AnonymousStruct *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badFRTRetain")))
    __attribute__((swift_attr("release:badFRTRelease")))
BadFRT {};
// expected-error@-1 {{specified retain function 'badFRTRetain' is not a function}}
// expected-error@-2 {{specified release function 'badFRTRelease' is not a function}}

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
// expected-error@-1 {{reference type 'MultipleRetainReleaseAttrFRT' must have exactly one 'retain:' Swift attribute}}
// expected-error@-2 {{reference type 'MultipleRetainReleaseAttrFRT' must have exactly one 'release:' Swift attribute}}

void retain1(MultipleRetainReleaseAttrFRT *v);
void retain2(MultipleRetainReleaseAttrFRT *v);
void release1(MultipleRetainReleaseAttrFRT *v);
void release2(MultipleRetainReleaseAttrFRT *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:")))
    __attribute__((swift_attr("release:emptyNameRelease")))
EmptyRetainName {};
// expected-error@-1 {{reference type 'EmptyRetainName' has an empty retain operation name}}

void emptyNameRelease(EmptyRetainName *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:emptyNameRetain")))
    __attribute__((swift_attr("release:")))
EmptyReleaseName {};
// expected-error@-1 {{reference type 'EmptyReleaseName' has an empty release operation name}}

void emptyNameRetain(EmptyReleaseName *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:Uretain")))
    __attribute__((swift_attr("release:Urelease")))
UnimportedRetainRelease {};
// expected-error@-1 {{cannot find retain function 'Uretain' for reference type 'UnimportedRetainRelease'}}
// expected-error@-2 {{cannot find release function 'Urelease' for reference type 'UnimportedRetainRelease'}}

void Uretain(UnimportedRetainRelease v);
UnimportedRetainRelease Urelease(UnimportedRetainRelease* v);

#pragma clang assume_nonnull end

//--- test.swift

import Test

public func test(x: NonExistent) { }
public func test(x: NoRetainRelease) { }
public func test(x: BadRetainRelease) { }
public func test(x: GoodRetainRelease) { }
public func test(x: GoodRetainReleaseWithRetainReturningSelf) { }
public func test(x: GoodRetainReleaseWithNullabilityAnnotations) { }
public func test(x: BadRetainRelease2) { }
public func test(x: BadRetainRelease3) { }
public func test(x: BadRetainRelease4) { }
public func test(x: DerivedFRT) { }
public func test(x: RefCountedDerived) { }
public func test(x: Base3FRT) { }
public func test(x: Base4FRT) { }
public func test(x: AnonymousStruct) { }
public func test(x: BadAnonymousStruct) { }
public func test(x: BadFRT) {}
public func test(x: MultipleRetainReleaseFRT) {}
public func test(x: MultipleRetainReleaseAttrFRT) {}
public func test(x: EmptyRetainName) {}
public func test(x: EmptyReleaseName) {}
public func test(x: UnimportedRetainRelease) {}
