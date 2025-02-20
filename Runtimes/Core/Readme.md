# SwiftCore

SwiftCore contains core libraries that sit below the platform overlays.
These include the standard library and associated runtimes, SwiftOnoneSupport,
and the concurrency runtimes.

## Build Instructions

> [!IMPORTANT]
> The standard library requires that it is built with a Swift compiler that is
> at least as new as the standard library sources. You will likely need to
> build the compiler as you would normally.
> In these instructions, `<swiftc>` is the path to your just-built Swift
> compiler.

First create a build directory in the `Core/` directory and navigate to it.

```sh
mkdir build && cd build
```

Configure a simple default build. This results in a static build of the standard
library.

```sh
cmake -G 'Ninja' -DCMAKE_Swift_COMPILER=<swiftc> ../`
ninja
```

To build a dynamic library, pass `-DBUILD_SHARED_LIBS=YES` to CMake.

```sh
cmake . -DBUILD_SHARED_LIBS=YES
ninja
```

To install the standard library to a staging directory, set the `DESTDIR`
environment variable and run `ninja install`

```sh
DESTDIR=SwiftCore-Staging ninja install
```

Semantic editing support is nice during development. CMake 3.29 and newer can
produce `compile_commands.json` compilation databases, which tools like
SourceKit-LSP and clangd use to provide a semantic editing experience with
features like semantic syntax highlighting, symbol hover information, and
jump-to-definition.

```sh
cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=YES
```

> [!NOTE]
> Some editors will look under `<source-root>/build` for the generated
> `compile_commands.json` file while others do not. You may want to create a
> symlink from the `Core/` directory to `build/compile_commands.json`.
> `ln -s build/compile_commands.json`

The compile-commands are specific to the current build configuration, so the
semantic results shown in the editor match what you are currently building.

There are many more knobs for configuration. From the build directory, run
`ccmake` to edit the build configuration to your liking.

### Reproducing a Build

The knobs make it easy to build the standard library to your needs, but can make
it challenging to reproduce a specific build configuration.
CMake has cache files to specify how to position the knobs for a given
configuration.

These caches live under `cmake/caches`, but can live anywhere.
To reproduce the Apple intel macOS standard library build, one would use the
following CMake command from the build directory:

```sh
cmake -G 'Ninja' \
  -DCMAKE_OSX_DEPLOYMENT_TARGET=15.3 \
  -DCMAKE_OSX_SYSROOT=macosx \
  -DCMAKE_Swift_COMPILER=<swiftc> \
  --toolchain ../cmake/caches/Vendors/Apple/Darwin.toolchain.cmake \
  -C ../cmake/cache/Vendors/Apple/x86_64-MacOSX.cmake \
  ../
ninja
```

> [!NOTE]
> Variables are evaluated in order of appearance on the command line.
> The `x86_64-MacOSX.cmake` cache requires that `CMAKE_OSX_DEPLOYMENT_TARGET` is
> set before usage. Therefore, setting `CMAKE_OSX_DEPLOYMENT_TARGET` must come
> before specifying the cache or you will get an error message.
