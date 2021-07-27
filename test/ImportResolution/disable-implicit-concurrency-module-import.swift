// RUN: %target-typecheck-verify-swift  -disable-implicit-concurrency-module-import
// REQUIRES: concurrency


protocol P : Actor { } // expected-error{{cannot find type 'Actor' in scope}}