// RUN: %target-parse-verify-swift


#if DEBUG

#endif ; // expected-error {{extra tokens at the end of the build configuration directive}}

// <rdar://problem/17569958> #endif not parsed correctly when occurs as last line in file
#if DEBUG
func debug(msg: String) {
    print(msg)
}
#else
func debug(msg: String) {
}
#endif // last line in file