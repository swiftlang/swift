// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep cfuncs.pcm
import cfuncs

func test_abort() {
  cfunc1() // FIXME:expected-error{{use of unresolved identifier 'cfunc1'}}
}


