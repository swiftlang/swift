#pragma once
#include <cstdio>

struct MoveOnly {
    int id;
    MoveOnly() : id(0) { printf("MoveOnly %d created\n", id); }
    MoveOnly(const MoveOnly&) = delete;
    MoveOnly(MoveOnly&& other) : id(other.id + 1) { printf("MoveOnly %d move-created\n", id); }
    ~MoveOnly() { printf("MoveOnly %d destroyed\n", id); }
};

struct Copyable {
    int id;
    Copyable() : id(0) { printf("Copyable %d created\n", id); }
    Copyable(const Copyable& other) : id(other.id + 1) { printf("Copyable %d copy-created\n", id); }
    Copyable(Copyable&& other) : id(other.id + 1) { printf("Copyable %d move-created\n", id); }
    ~Copyable() { printf("Copyable %d destroyed\n", id); }
};

inline void byRValueRef(MoveOnly&& x) {}
inline void byRValueRef(Copyable&& x) {}

// An rvalue-qualified 'this' takes the same route as an rvalue-reference
// parameter: the callee gets a pointer, the caller destroys.
struct RValueThis {
    int id;
    RValueThis() : id(0) { printf("RValueThis %d created\n", id); }
    RValueThis(const RValueThis& other) : id(other.id + 1) { printf("RValueThis %d copy-created\n", id); }
    RValueThis(RValueThis&& other) : id(other.id + 1) { printf("RValueThis %d move-created\n", id); }
    ~RValueThis() { printf("RValueThis %d destroyed\n", id); }
    void onRValue() && { printf("RValueThis %d onRValue\n", id); }
};

// Overloading along the ref-qualifier axis renames the rvalue one on import.
struct RefQualifiedThis {
    int id;
    RefQualifiedThis() : id(0) { printf("RefQualifiedThis %d created\n", id); }
    RefQualifiedThis(const RefQualifiedThis& other) : id(other.id + 1) { printf("RefQualifiedThis %d copy-created\n", id); }
    RefQualifiedThis(RefQualifiedThis&& other) : id(other.id + 1) { printf("RefQualifiedThis %d move-created\n", id); }
    ~RefQualifiedThis() { printf("RefQualifiedThis %d destroyed\n", id); }
    void method() const & { printf("RefQualifiedThis %d lvalue\n", id); }
    void method() && { printf("RefQualifiedThis %d rvalue\n", id); }
};

// A move-only receiver: the call must move out of it, not copy.
struct MoveOnlyThis {
    int id;
    MoveOnlyThis() : id(0) { printf("MoveOnlyThis %d created\n", id); }
    MoveOnlyThis(const MoveOnlyThis&) = delete;
    MoveOnlyThis(MoveOnlyThis&& other) : id(other.id + 1) { printf("MoveOnlyThis %d move-created\n", id); }
    ~MoveOnlyThis() { printf("MoveOnlyThis %d destroyed\n", id); }
    void onRValue() && { printf("MoveOnlyThis %d onRValue\n", id); }
};

// A type imported as a class is itself the reference, so 'this' is passed
// directly and nothing is copied. The reference count is real: an unbalanced
// release traps, and the destructor prints so the object's end is observable.
struct RefThis {
    mutable int refs = 1;

    __attribute__((swift_attr("returns_retained")))
    RefThis() { printf("RefThis created\n"); }
    ~RefThis() { printf("RefThis destroyed\n"); }

    void retain() const { check(); ++refs; }
    void release() const { --refs; check(); if (refs == 0) delete this; }
    void check() const { if (refs < 0) __builtin_trap(); }

    void onRValue() && { printf("RefThis onRValue\n"); }
} __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")));

template <typename T>
struct UniversalRef {
  inline void byRValueRef(T &&x) {}
};

using UniversalMoveOnly = UniversalRef<MoveOnly>;
using UniversalCopyable = UniversalRef<Copyable>;
