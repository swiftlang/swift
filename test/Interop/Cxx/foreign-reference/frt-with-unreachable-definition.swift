// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// Make sure we get diagnostics when FRT reachability is an issue:
//
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}verify.swift \
// RUN:   -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}FRTImpl.h \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}OpsImpl.h \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}CxxModule.h
//
// Make sure the program compiles when FRT reachability isn't an issue:
//
// RUN: %target-swift-frontend -typecheck %t/compile.swift \
// RUN:   -I %t/Inputs -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking

//--- Inputs/module.modulemap
module CxxModule {
    textual header "Annotations.h"

    // Full definitions of FRTBase and FRTDerived live here, but because this
    // submodule is explicit, those definitions are not reachable to Swift when
    // it has `import CxxModule`.
    explicit module Definition {
        header "FRTImpl.h"
        export *
    }

    // Free-function retain/release for FRTOps live here.
    explicit module OpsImpl {
        header "OpsImpl.h"
        export *
    }

    // What Swift "sees"
    header "CxxModule.h"
    export *
}

//--- Inputs/Annotations.h
#pragma once

#define SWIFT_SHARED_REFERENCE(_retain, _release)                              \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:" #_retain)))                              \
  __attribute__((swift_attr("release:" #_release)))

#define SWIFT_RETURNED_AS_UNRETAINED_BY_DEFAULT                                \
  __attribute__((swift_attr("returned_as_unretained_by_default")))

//--- Inputs/FRTImpl.h
#pragma once
#include <cstddef> // for size_t
#include "Annotations.h"

class FRTBasic {
  mutable int m_count = 0;
  FRTBasic(const FRTBasic &) = delete;
  FRTBasic &operator=(const FRTBasic &) = delete;
public:
  FRTBasic() = default;
  virtual ~FRTBasic() = default;
  void ref() const;
  void deref() const;
  void *operator new(size_t size);
  void operator delete(void *ptr);
} SWIFT_SHARED_REFERENCE(.ref, .deref)
  SWIFT_RETURNED_AS_UNRETAINED_BY_DEFAULT;

template <typename T>
class FRTBase {
  mutable int m_count = 0;
  FRTBase(const FRTBase &) = delete;
  FRTBase &operator=(const FRTBase &) = delete;
public:
  FRTBase() = default;
  virtual ~FRTBase() = default;
  void ref() const { ++m_count; }
  void deref() const {
    if (--m_count < 0) __builtin_trap();
    if (m_count == 0) delete static_cast<T *>(const_cast<FRTBase *>(this));
  }
  void *operator new(size_t size) { static FRTBase f{}; return &f; }
  void operator delete(void *ptr) {}
} SWIFT_SHARED_REFERENCE(.ref, .deref)
  SWIFT_RETURNED_AS_UNRETAINED_BY_DEFAULT;

// expected-error@+1 {{has unreachable definition}}
class FRTDerived : public FRTBase<FRTDerived> {
  FRTDerived(const FRTDerived &) = delete;
  FRTDerived &operator=(const FRTDerived &) = delete;
public:
  FRTDerived() = default;
  void *operator new(size_t size) { static FRTDerived f{}; return &f; }
  void operator delete(void *ptr) {}
};

class FRTAnnotated : public FRTBase<FRTAnnotated> {
  FRTAnnotated(const FRTAnnotated &) = delete;
  FRTAnnotated &operator=(const FRTAnnotated &) = delete;
public:
  FRTAnnotated() = default;
  void *operator new(size_t size) { static FRTAnnotated f{}; return &f; }
  void operator delete(void *ptr) {}
} SWIFT_SHARED_REFERENCE(.ref, .deref)
  SWIFT_RETURNED_AS_UNRETAINED_BY_DEFAULT;

//--- Inputs/OpsImpl.h
#pragma once

class FRTOps;

void retainDirect(FRTOps *);
void releaseDirect(FRTOps *);

//--- Inputs/CxxModule.h
#pragma once
#include <cstddef>
#include "Annotations.h"

class FRTBasic;
FRTBasic *makeFRTBasic();

class FRTDerived;
FRTDerived *makeFRTDerived();

class FRTAnnotated;
FRTAnnotated *makeFRTAnnotated();

// FRTOps is fully defined here and thus reachable, but its retain/release
// functions are only declared in the OpsImpl submodule and thus unreachable.
// expected-error@+2 {{cannot find retain function 'retainDirect' for reference type 'FRTOps'}}
// expected-error@+1 {{cannot find release function 'releaseDirect' for reference type 'FRTOps'}}
class FRTOps {
public:
  FRTOps() = default;
  void *operator new(size_t size);
  void operator delete(void *ptr);
} SWIFT_SHARED_REFERENCE(retainDirect, releaseDirect)
  SWIFT_RETURNED_AS_UNRETAINED_BY_DEFAULT;

FRTOps *makeFRTOps();

//--- verify.swift
import CxxModule

let _ = makeFRTBasic()
let _ = makeFRTDerived()
let _ = makeFRTOps()
let _ = makeFRTAnnotated()

//--- compile.swift
import CxxModule

// This works because we support FRTs of forward-declared classes, as long as
// they aren't FRTs as a result of inheritance.
func makeBasic() -> FRTBasic {
  return makeFRTBasic()
}

// This still works because even though FRTAnnotated inherits from an FRT base,
// it has been re-annotated.
func makeAnnotated() -> FRTAnnotated {
  return makeFRTAnnotated()
}
