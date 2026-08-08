// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify \
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

#define SWIFT_SHARED_REFERENCE(_retain, _release)                              \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:" #_retain)))                              \
  __attribute__((swift_attr("release:" #_release)))

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
    __attribute__((swift_attr("retain:badRetainReturn")))
    __attribute__((swift_attr("release:badReleaseReturn")))
BadRetainReleaseReturn {};
// expected-error@-1 {{release function 'badReleaseReturn' must return an integer or 'void'}}
// expected-error@-2 {{retain function 'badRetainReturn' must return an integer, its parameter type, or 'void'}}

float badRetainReturn(BadRetainReleaseReturn *v);
void *badReleaseReturn(BadRetainReleaseReturn *v);

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

struct nonCXXFRT{};

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetainParam1")))
    __attribute__((swift_attr("release:badReleaseParam1")))
BadRetainReleaseParam1 {};
// expected-error@-1{{release function 'badReleaseParam1' must have exactly one argument of type 'BadRetainReleaseParam1'}}
// expected-error@-2{{retain function 'badRetainParam1' must have exactly one argument of type 'BadRetainReleaseParam1'}}

void badRetainParam1(nonCXXFRT *);
void badReleaseParam1(nonCXXFRT *);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetainParam2")))
    __attribute__((swift_attr("release:badReleaseParam2")))
BadRetainReleaseParam2 {};
// expected-error@-1 {{release function 'badReleaseParam2' must have exactly one argument of type 'BadRetainReleaseParam2'}}
// expected-error@-2 {{retain function 'badRetainParam2' must have exactly one argument of type 'BadRetainReleaseParam2'}}

void badRetainParam2(int);
void badReleaseParam2(int);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:badRetainParam3")))
    __attribute__((swift_attr("release:badReleaseParam3")))
BadRetainReleaseParam3 {};
// expected-error@-1 {{release function 'badReleaseParam3' must have exactly one argument of type 'BadRetainReleaseParam3'}}
// expected-error@-2 {{retain function 'badRetainParam3' must have exactly one argument of type 'BadRetainReleaseParam3'}}

void badRetainParam3(BadRetainReleaseParam2 *);
void badReleaseParam3(BadRetainReleaseParam2 *);

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
// expected-error@-1 {{retain function 'base4FRTRetain' must have exactly one argument of type 'Base4FRT'}}

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
// expected-error@-5{{release function 'badAnonymousStructRelease' must have exactly one argument of type 'BadAnonymousStruct'}}
// expected-error@-6{{retain function 'badAnonymousStructRetain' must have exactly one argument of type 'BadAnonymousStruct'}}

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
    // expected-note@-1{{retain function specified on 'MultipleRetainReleaseAttrFRT'}}
    __attribute__((swift_attr("retain:retain1")))
    // expected-note@-1{{retain function specified on 'MultipleRetainReleaseAttrFRT'}}
    __attribute__((swift_attr("release:release1")))
    // expected-note@-1{{release function specified on 'MultipleRetainReleaseAttrFRT'}}
    __attribute__((swift_attr("release:release2")))
    // expected-note@-1{{release function specified on 'MultipleRetainReleaseAttrFRT'}}
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
    // expected-note@-1{{retain function specified on 'EmptyRetainName'}}
    __attribute__((swift_attr("release:emptyNameRelease")))
EmptyRetainName {};
// expected-error@-1 {{reference type 'EmptyRetainName' has an empty retain operation name}}

void emptyNameRelease(EmptyRetainName *v);

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:emptyNameRetain")))
    __attribute__((swift_attr("release:")))
    // expected-note@-1{{release function specified on 'EmptyReleaseName'}}
EmptyReleaseName {};
// expected-error@-1 {{reference type 'EmptyReleaseName' has an empty release operation name}}

void emptyNameRetain(EmptyReleaseName *v);

// This struct is formatted with roughly one token per line to make it clear which token the diagnostic is pointed to
struct
  MixedImmortalHeaderLoc
  // expected-error@-1:3 {{reference type 'MixedImmortalHeaderLoc' must mark both or neither of its retain and release operations as immortal}}
  {
  }
  SWIFT_SHARED_REFERENCE(mixedImmortalRetain, immortal)
  // expected-note@-1:3 {{retain and release functions specified on 'MixedImmortalHeaderLoc'}}
;

void mixedImmortalRetain(MixedImmortalHeaderLoc *v);

struct
  MixedImmortalAPINotes
  // expected-error@-1:3 {{reference type 'MixedImmortalAPINotes' must mark both or neither of its retain and release operations as immortal}}
  {
  }
;
// N.B. attributes come from Test.apinotes and carry no source location, so no note is emitted

void apiNotesMixedImmortalRelease(MixedImmortalAPINotes *v);

void FRTParameterByValue(GoodRetainRelease v={});
// expected-note@-1{{parameter takes foreign reference type 'GoodRetainRelease' by value which violates reference type contract}}
GoodRetainRelease FRTReturnByValue(GoodRetainRelease* v);
// expected-note@-1{{function returns foreign reference type 'GoodRetainRelease' by value which violates reference type contract}}

#pragma clang assume_nonnull end

//--- Inputs/Test.apinotes
---
Name: Test
Tags:
- Name: MixedImmortalAPINotes
  SwiftImportAs: reference
  SwiftRetainOp: immortal
  SwiftReleaseOp: apiNotesMixedImmortalRelease

//--- test.swift

import Test

public func test(x: NonExistent) { }
public func test(x: NoRetainRelease) { }
public func test(x: BadRetainReleaseReturn) { }
public func test(x: GoodRetainRelease) { }
public func test(x: GoodRetainReleaseWithRetainReturningSelf) { }
public func test(x: GoodRetainReleaseWithNullabilityAnnotations) { }
public func test(x: BadRetainReleaseParam1) { }
public func test(x: BadRetainReleaseParam2) { }
public func test(x: BadRetainReleaseParam3) { }
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
public func test(x: MixedImmortalHeaderLoc) {}
public func test(x: MixedImmortalAPINotes) {}

public func testCalls() {
  FRTParameterByValue() // expected-error {{cannot find 'FRTParameterByValue' in scope}}
  FRTReturnByValue() // expected-error {{cannot find 'FRTReturnByValue' in scope}}
}
