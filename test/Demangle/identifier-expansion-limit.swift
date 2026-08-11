; This is not really a Swift source file: -*- Text -*-

; Ensure that an identifier whose word substitutions expand past the
; demangler's length limit is rejected gracefully: the name is passed through
; unchanged, with no crash, hang, or fatal error.

; We need python to build the input.
UNSUPPORTED: OS=windows-msvc

RUN: %empty-directory(%t)

; 2048 substitutions of a 2048-character word is exactly the 4 MiB limit.
RUN: %{python} %S/Inputs/gen_word_substitution.py 2048 > %t/over.txt
RUN: swift-demangle < %t/over.txt 2>&1 | %FileCheck %s --check-prefix=OVER
OVER: $s02048xxx
OVER-SAME: 3fooV

; One substitution fewer stays under the limit and still demangles.
RUN: %{python} %S/Inputs/gen_word_substitution.py 2047 > %t/under.txt
RUN: swift-demangle < %t/under.txt 2>&1 | %FileCheck %s --check-prefix=UNDER
UNDER: xxx
UNDER-SAME: .foo
