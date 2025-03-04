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
