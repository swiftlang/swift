int conflict;
typedef int conflict;

#line 42 "a-fake-file.h"
int conflict2;
typedef int conflict2;

#define I __extension__ conflict
int I;

// Purposely missing a newline at EOF in order to test diagnostics with
// a EOF location
int lastLineOfFileWithoutSemicolon