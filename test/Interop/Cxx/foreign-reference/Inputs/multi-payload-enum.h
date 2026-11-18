#pragma once
#include "swift/bridging"

struct SWIFT_SHARED_REFERENCE(retainA, releaseA) A { int refCount = 1; };
struct SWIFT_SHARED_REFERENCE(retainB, releaseB) B { int refCount = 1; };

inline void retainA(A *a) { ++a->refCount; }
inline void releaseA(A *a) { if (!--a->refCount) delete a; }
inline void retainB(B *b) { ++b->refCount; }
inline void releaseB(B *b) { if (!--b->refCount) delete b; }

inline A *makeA() SWIFT_RETURNS_RETAINED { return new A; }
inline B *makeB() SWIFT_RETURNS_RETAINED { return new B; }
