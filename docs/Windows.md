# Getting Started with Swift on Windows

One can build and run Swift natively, or through the Windows Subsystem for 
Linux.

## Native Windows

Currently there are three supported ways to build Swift for Windows.

1. To cross-compile Swift for Windows from another host operating system (using
   `clang`), see [Cross Compiling for Windows](./WindowsCrossCompile.md)

1. To build on Windows using `clang-cl`, see [Building on
   Windows](./WindowsBuild.md#clang-cl)

1. To build on Windows using Microsoft Visual C++ (MSVC), see [Building on 
   Windows](./WindowsBuild.md#MSVC)

`clang-cl` is recommended over MSVC for building Swift on Windows.
Although it is possible to build the compiler and the standard library with
MSVC to use those built products to compile a Swift program, it won't be
possible to run the binary without seperately obtaining the Swift runtime. On
the other hand, `clang-cl` is able to build the runtime, which makes it
possible to build and run all the components required for Swift natively on
Windows.

## Windows Subsystem for Linux

On the [Windows Subsystem for
Linux](https://docs.microsoft.com/en-us/windows/wsl/about), it's possible to
build and run Swift in a Linux-like environment, on Windows. See [Swift on 
the Windows Subsystem for Linux](./WindowsSubsystemForLinux.md) for details.
