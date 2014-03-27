// RUN: %swift -parse %s -verify

// With a space next to the '#if'
#if 
class C {} // expected-error{{expected #else or #endif at end of configuration block}}
#endif

