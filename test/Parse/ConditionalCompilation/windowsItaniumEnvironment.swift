// RUN: %swift -parse %s -target i686-unknown-windows-itanium -parse-as-library -parse-stdlib -verify

#if _environment(itanium)
public class itanium {
}
#endif

let instance = itanium()

