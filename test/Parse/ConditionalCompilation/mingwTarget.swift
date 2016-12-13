// RUN: %swift -parse %s -target i686-unknown-windows-gnu -parse-as-library -parse-stdlib -verify

#if _environment(gnu)
public class MinGW {
}
#endif

let instance = MinGW()

