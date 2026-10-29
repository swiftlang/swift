// RUN: %target-typecheck-verify-swift -parse-as-library
// REQUIRES: OS_BINFMT=pe-coff

// The index brackets a section's records with markers in its "$A" and "$Z"
// groups, so the records need a "$" group that the linker sorts between them.
// The warning is for records meant to be found: '@used' variables.

@section(".myplugs$B") @used var ok0: Int = 1
@section(".myplugs$Y") @used var ok1: Int = 2
@section(".myplugs$AB") @used var ok2: Int = 3
@section(".mycode$B") @used func ok3() {}

@section(".myplugs") @used var bad0: Int = 4 // expected-warning {{section name '.myplugs' has no '$' group that sorts between '$A' and '$Z', so the section will not be listed in the image's section index}}
@section(".myplugs$A") @used var bad1: Int = 5 // expected-warning {{section name '.myplugs$A' has no '$' group that sorts between '$A' and '$Z', so the section will not be listed in the image's section index}}
@section(".myplugs$Z") @used var bad2: Int = 6 // expected-warning {{section name '.myplugs$Z' has no '$' group that sorts between '$A' and '$Z', so the section will not be listed in the image's section index}}
@section(".myplugs$b") @used var bad3: Int = 7 // expected-warning {{section name '.myplugs$b' has no '$' group that sorts between '$A' and '$Z', so the section will not be listed in the image's section index}}
@section("$B") @used var bad4: Int = 8 // expected-warning {{section name '$B' has no '$' group that sorts between '$A' and '$Z', so the section will not be listed in the image's section index}}

// Functions are not indexed, and a variable that is not '@used' is not
// expected to be found, so neither warns.
@section(".mycode") @used func quiet0() {}
@section(".myplugs") var quiet1: Int = 9
