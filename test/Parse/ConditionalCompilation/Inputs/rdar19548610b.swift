class C1 {}
// expected-cparser-error@+2{{unexpected conditional compilation block terminator}}
// expected-astgen-error@+1{{unexpected code}}
#else
class C2 {}
// expected-cparser-error@+1{{unexpected conditional compilation block terminator}}
#endif
class C3 {}
