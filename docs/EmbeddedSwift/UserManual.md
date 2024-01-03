# Embedded Swift -- User Manual

**⚠️ Embedded Swift is experimental. This document might be out of date with latest development.**

**‼️ Use the latest downloadable 'Trunk Development' snapshot from swift.org to use Embedded Swift. Public releases of Swift do not yet support Embedded Swift.**

For an introduction and motivation into Embedded Swift, please see "[A Vision for Embedded Swift](https://github.com/apple/swift-evolution/blob/main/visions/embedded-swift.md)", a Swift Evolution document highlighting the main goals and approaches.

The following document explains how to use Embedded Swift's support in the Swift compiler and toolchain.

## What Embedded Swift is, and what it isn't

- Embedded Swift **is** a way to produce small and freestanding binaries (with no, or trivial dependencies).
- Embedded Swift **is not** a complete one-click solution to program all embedded boards and MCUs.
- Embedded Swift **is** a compilation model that's analogous to a traditional C compiler in the sense that the compiler produces an object file (.o) that can be simply linked with your existing code, and it's not going to require you to port any libraries or runtimes.
- Embedded Swift **is not** a HAL, it's not an SDK for development, it's not a set of libraries to program peripherals using high-level APIs. It's instead a compilation mode that's suitable for creating these components.

## Using Embedded Swift

A typical setup and build + run cycle for an embedded development board involves:

- (1) Getting an SDK with the C compilers, headers and libraries for the target
- (2) Building the C source code, and Swift source code
- (3) Linking all the libraries, C object files, and Swift object files.
- (4) Post-processing the linked firmware into a flashable format (UD2, BIN, or bespoke formats)
- (5) Uploading the flashable binary to the board over a USB cable using some vendor-provided JTAG/SWD tool or by copying it to a fake USB Mass Storage volume presented by the board.
- (6) Restarting the board, observing physical effects of the firmware (LEDs light up) or UART output over USB, or presence on network, etc.

Most of these steps are out of scope for this document, instead refer to the vendor provided documentation and get familiar with the details of firmware development for your board without Swift in the mix first. Even if you want to build a completely pure Swift firmware, you are still very likely going to need the the vendor provided tooling for linking, post-processing, uploading, etc.

## Building code using Embedded Swift

A basic way to build a set of Swift source files in Embedded Swift mode, is to simply give the compiler (1) a target triple, (2) the `-enable-experimental-feature Embedded` flag, (3) the set of source files that form the input module:

```bash
$ swiftc -target <target triple> -enable-experimental-feature Embedded \
  input1.swift input2.swift ... -c -o output.o
```

The target triple also decides whether whether the output object file will be an ELF file, or a Mach-O. For example:

```bash
# To build an ARMv7 Mach-O object file:
$ swiftc -target armv7-apple-none-macho -enable-experimental-feature Embedded \
  input1.swift input2.swift ... -c -o output.o

# To build an ARMv7 ELF object file:
$ swiftc -target armv7-unknown-none-eabi -enable-experimental-feature Embedded \
  input1.swift input2.swift ... -c -o output.o
```

Additionally, you probably want to specify additional Clang and/or LLVM flags to get the compiler to produce code for the exact ISA and ABI you need for your target. For example, a Raspberry Pi Pico / Pico W expects the `-mfloat-abi=soft` Clang option, and if you want to match ABI with libraries built with the GNU toolchain, you might also need `-fshort-enums`. To pass those to Swift, use the `-Xcc` prefix:

```bash
# To build an ELF object file for ARMv6-M with soft float ABI (floating-point arguments passed in integer registers) and "short enums":
$ swiftc -target armv6m-unknown-none-eabi -enable-experimental-feature Embedded \
   -Xcc -mfloat-abi=soft -Xcc -fshort-enums \
  input1.swift input2.swift ... -c -o output.o
```

This might not be obvious: `-Xcc` flags are typically only used to alter behavior of the Clang importer, but passing flags to Clang this way also works to specify LLVM target options like selecting a specific CPU architecture (`-march`, `-mcpu`, `-mmcu`), FPU unit availability (`-mfpu`), which registers are used to pass floating-point values (`-mfloat-abi`), and others.

## Examples

### Building a pure Swift firmware for an embedded target

TODO

### Integrating with embedded SDKs and build systems

TODO

### Building a macOS Embedded Swift program:

It's also possible to build in Embedded Swift mode for regular non-embedded operating systems, like macOS. This is very useful for testing purposes, or if you just want to observe and experiment with Embedded Swift. A simple source code like this:

```swift
print("Hello, embedded world!")
```

...can be compiled using the `-enable-experimental-feature Embedded` flag (the implicit `-target` matches the host OS):

```bash
$ xcrun swiftc hello.swift -enable-experimental-feature Embedded -wmo
$ ./hello
Hello, embedded world!
```

Note that the resulting executable is still a *dynamically-linked executable*, so it's not fully standalone in the embedded sense. Namely is still uses `putchar` from Libsystem. But the singular object file that was used to build this executable was produced by the compiler in the same fashion that a real embedded build would. If we ask the compiler and linker to minimize the size of the outputs and to remove any unused code, we can observe that the binary has no other dependencies other than `putchar` and that the machine code section is very small (172 bytes in the `__text` section):

```bash
$ xcrun swiftc hello.swift -enable-experimental-feature Embedded -wmo -Osize -Xlinker -dead_strip
$ nm -um ./hello
                 (undefined) external _putchar (from libSystem)
$ size -m ./hello
Segment __TEXT: 16384
  Section __text: 172
...
```

## Embedded Swift is a subset of Swift

Embedded Swift is a subset of the Swift language, and some features are not available in Embedded Swift, however features are available, including: Generics, protocols, enums with associated values, tuples, optionals, classes (instances are allocated on the heap and refcounted just like in regular Swift), inheritance, runtime polymorphism, arrays (heap-allocated copy-on-write just like in regular Swift) and many more.

Features that are not available:

- **Not available**: Runtime reflection (`Mirror` APIs).
- **Not available**: Values of protocol types ("existentials"), e.g. `let a: Hashable = ...`, are not allowed. `Any` and `AnyObject` are also not allowed.
- **Not available**: Metatypes, e.g. `let t = SomeClass.Type` or `type(of: value)` are not allowed.
- **Not available yet (under development)**: The print() function for types other than StaticString and integers.
- **Not available yet (under development)**: String. (StaticString **is** available).
- **Not available yet (under development)**: Set.
- **Not available yet (under development)**: Dictionary.

## Libraries and modules in Embedded Swift

TODO

## Allocating and non-allocating Embedded Swift mode

TODO

## Runtime support for allocating Embedded Swift

TODO

## External dependencies

Embedded Swift minimizes external dependencies, but they still exist. In Embedded Swift compilation mode, the compiler only triggers external dependencies based on actual usage of the program under compilation. The following table lists which situations cause which external dependencies:

|                                                                     | external dependencies:                                                  |
|---------------------------------------------------------------------|-------------------------------------------------------------------------|
| *(basic dependencies, added by the compiler in various situations)* | `void *memset(void *, int, size_t);`                                    |
| instantiating a class, or using UnsafePointer.allocate()            | `int posix_memalign(void **, size_t, size_t);`<br/>`void free(void *);` |
| using print()                                                       | `int putchar(int);`                                                     |
| using Hashable, Set, Dictionary, or random-number generating APIs   | `void arc4random_buf(void *, size_t);`                                  |

The user and/or the platform is expected to provide these well-known APIs.
