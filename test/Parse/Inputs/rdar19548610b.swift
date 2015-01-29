class C1 {}
// expected-error@+1{{unexpected configuration block terminator}}
#else
class C2 {}
// expected-error@+1{{unexpected configuration block terminator}}
#endif
class C3 {}
