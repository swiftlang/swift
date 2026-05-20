#pragma once

struct TemplatedReturningInt {
    int data[4] = {};
    template<typename I> int &operator[](I index) { return data[index]; }
};

struct TemplatedTakingInt {
    int data[4] = {};
    // This template doesn't instantiate unless I is int. Bogus but valid.
    template<typename I> I &operator[](int x) { return data[x]; }
};
