// RUN: %swift -parse %s -target i686-unknown-windows-msvc -parse-as-library -parse-stdlib -verify

#if _environment(msvc)
public class msvc {
}
#endif

let instance = msvc()

