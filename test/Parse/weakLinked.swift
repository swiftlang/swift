// RUN: %swift -target x86_64-unknown-windows-msvc -parse-stdlib -typecheck -verify %s

// expected-error@+1{{attribute @_weakLinked is unsupported on target 'x86_64-unknown-windows-msvc'}}
@_weakLinked func f() { }

