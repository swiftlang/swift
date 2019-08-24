class C1 {}
// expected-error@+1{{unexpected conditional compilation block terminator}}
#else
class C2 {}
// expected-error@+1{{unexpected conditional compilation block terminator}}
#endif
class C3 {}
