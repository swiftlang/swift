# Resilient Library Evolution Tests

This directory tests for the correctness of *resilience*, which is a
broad term for Swift maximizing binary compatibility of a dependency
while maintaining the freedom to do things that would normally break
  clients in other languages, such as changing the layout of nominal
  types. The detailed explanation of resilience is out of scope of this
  little readme.

Each main test file should compile against an "old" version of a
module/library, and a "new" version.

There are four valid combinations for each test:

1. Main file compiled against old library, linked against old library
   a.k.a. "beforebefore"
2. Main file compiled against old library, linked against new library,
   a.k.a. "beforeafter"
3. Main file compiled against new library, linked against old library,
   a.k.a. "afterbefore"
4. Main file compiled against new library, linked against new library,
   a.k.a. "afterafter"

Compiling the main file determines which declarations and transparent
function bodies are available when deserializing the library's
swiftmodule. When linking with the library, binary compatibility should
be maintained.

In the main file, use your test library's `getVersion` helper function
to know which outputs to expect.

When adding a new test, see the boilerplate at the top of each file for
the general pattern for this kind of test. Use the `StdlibUnittest`
library.

