#pragma once
#include <iostream>

struct MoveOnly {
    int id;
    MoveOnly() : id(0) { std::cout << "MoveOnly " << id << " created\n"; }
    MoveOnly(const MoveOnly&) = delete;
    MoveOnly(MoveOnly&& other) : id(other.id + 1) { std::cout << "MoveOnly " << id << " move-created\n"; }
    ~MoveOnly() { std::cout << "MoveOnly " << id << " destroyed\n"; }
};

struct Copyable {
    int id;
    Copyable() : id(0) { std::cout << "Copyable " << id << " created\n"; }
    Copyable(const Copyable& other) : id(other.id + 1) { std::cout << "Copyable " << id << " copy-created\n"; }
    Copyable(Copyable&& other) : id(other.id + 1) { std::cout << "Copyable " << id << " move-created\n"; }
    ~Copyable() { std::cout << "Copyable " << id << " destroyed\n"; }
};

inline void byRValueRef(MoveOnly&& x) {}
inline void byRValueRef(Copyable&& x) {}
