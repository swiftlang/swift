// RUN: %target-typecheck-verify-swift

/* outer block comment starts here expected-note{{comment started here}}
  /* inner block comment starts and ends here */
/* a second inner block started here
/* another inner block comment starts here
   Note that there are now three open comment blocks.
   The error message on the next line bumps that to four,
   then the fix-it text lowers that to two.
   expected-error{{unterminated '/*' comment}} {{62-62=*/*/}}
