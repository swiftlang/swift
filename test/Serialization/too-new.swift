// RUN: %swift %s -parse -I=%S/Inputs -verify

import too_new // expected-error{{module file was created by a newer version of the compiler}}

too_new // no-warning (but empty)
