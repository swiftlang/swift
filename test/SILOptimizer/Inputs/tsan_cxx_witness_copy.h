#pragma once

struct Ref {};

struct RefVec {
    Ref* m_buffer;
    int m_size;

    RefVec() : m_buffer(nullptr), m_size(0) {}
    RefVec(const RefVec& o) : m_buffer(o.m_buffer), m_size(o.m_size) {}
    ~RefVec() {}

    int size() const { return m_size; }
    const Ref* at(int i) const { return &m_buffer[i]; }
};
