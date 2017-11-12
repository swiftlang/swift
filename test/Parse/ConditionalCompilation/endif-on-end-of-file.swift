// RUN: %target-typecheck-verify-swift


#if DEBUG

#endif ; // expected-error {{extra tokens following conditional compilation directive}}

// <rdar://problem/17569958> #endif not parsed correctly when occurs as last line in file
#if DEBUG
func debug(msg: String) {
    print(msg)
}
#else
func debug(msg: String) {
}
// last line of file - make sure no \n sneaks in below #endif.
#endif
