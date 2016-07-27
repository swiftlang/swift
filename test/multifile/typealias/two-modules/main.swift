// RUN: rm -rf %t && mkdir %t

// RUN: mkdir %t/linker
// RUN: %target-build-swift -emit-module -c %S/library.swift -o %t/linker/library.o
// RUN: %target-build-swift -emit-library -c %S/library.swift -o %t/linker/library.o
// RUN: %target-build-swift %S/main.swift %t/linker/library.o -I %t/linker/ -L %t/linker/ -o %t/linker/main

// RUN: %target-build-swift -g -emit-module -c %S/library.swift -o %t/linker/library.o
// RUN: %target-build-swift -g -emit-library -c %S/library.swift -o %t/linker/library.o
// RUN: %target-build-swift -g %S/main.swift %t/linker/library.o -I %t/linker/ -L %t/linker/ -o %t/linker/main

// REQUIRES: executable_test

import library

func testFunction<T>(withCompletion completion: (Result<T, Error>) -> Void) { }
testFunction { (result: GenericResult<Int>) in }
