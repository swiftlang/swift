// RUN: %target-parse-verify-swift

// e xpected-error@+1{{unexpected configuration expression type}}
#if _runtime(_ObjC)
// expected-error@+2{{expected '{' in protocol type}}
// expected-error@+1{{expected #else or #endif at end of configuration block}}
public protocol _CocoaString}
