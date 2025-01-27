// RUN: %empty-directory(%t2)
// RUN: %target-clangxx -fuse-ld=llvm-lib %S/Inputs/c++/main.cpp -c -o %t2/main.cpp.obj
// RUN: llvm-lib /OUT:%t2/c++.lib %t2/main.cpp.obj
// RUN: %target-run-simple-swift(-L %t2 -cxx-interoperability-mode=default -Xcc -cxx-isystem -Xcc %S/Inputs/c++ -Xcc -stdlib=libc++ -Xcc -nostdinc++ -I %S/Inputs)
//
// REQUIRES: executable_test
// REQUIRES: OS=windows-msvc

// NOTE: do not rely on StdlibUnittest as it might pull in 'std' on Windows due to custom test libc++ being incomplete.
import CxxHeader
import custom_std

func test() {
    let s = SimpleStruct(x: 0, y: 0)
    assert(s.x == 0)
    let str = std.string("hello")
    assert(str.size() == 5)
}

test()
