// RUN: %swift -parse %s -target x86_64-unknown-windows-cygnus -parse-as-library -parse-stdlib -verify

#if _environment(cygnus)
public class cygwin {
}
#endif

let instance = cygwin()

