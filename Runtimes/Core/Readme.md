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

Run these commands from the `Runtimes/Core` directory.

Build a standard library with the default configuration. This builds for the
system that the command was run on, usually resulting in a static archive
without optimizations applied.

```sh
cmake -B build -S . -G Ninja -DCMAKE_Swift_COMPILER=<swiftc>
cmake --build build
DESTDIR=/tmp/staging-dir cmake --install build --prefix /usr
```

The `DESTDIR` environment variable sets the staging location so that the result
of the build isn't installed directly to `/usr` on the system performing the
build.

> [!NOTE]
> The `DESTDIR` environment variable is not portable to Windows.
> Use `CMAKE_INSTALL_PREFIX` or the `--prefix` flag to set the staging location.

To build the runtimes as dynamic libraries, pass `-DBUILD_SHARED_LIBS=YES` to
CMake.

```sh
cmake -B build -S . -G Ninja -DBUILD_SHARED_LIBS=YES -DCMAKE_Swift_COMPILER=<swiftc>
cmake --build build
```

To enable semantic editing, ensure you're running CMake 3.29 or newer and enable
`CMAKE_EXPORT_COMPILE_COMMANDS`.

```sh
cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=YES
```

Some editors will look under `<source-root>/build` for the generated
`compile_commands.json` while others do not.
On UNIX-like systems, run `ln -s build/compile_commands.json`.
On Windows, creating symlinks requires administrator privileges, but can be done
with CMake `cmake -E create_symlink build\compile_commands.json
compile_commands.json`. If you do not have administrator privileges, you can
copy the file from your build directory into the root directory of your sources.
Note that the copied file won't be updated so you will need to copy the file
each time you re-run CMake.

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
The following command uses the `x86_64-MacOSX.cmake` cache file to reproduce the
Apple Intel macOS standard library build.


```sh
cmake -B build -S . -G Ninja \
    -DCMAKE_OSX_DEPLOYMENT_TARGET=15.3 \
    -DCMAKE_OSX_SYSROOT=macosx \
    -DCMAKE_Swift_COMPILER=<swiftc> \
    --toolchain cmake/caches/Vendors/Apple/Darwin.toolchain.cmake \
    -C cmake/caches/Vendors/Apple/x86_64-MacOSX.cmake
cmake --build build
```

> [!NOTE]
> Variables are evaluated in order of appearance on the command line.
> The `x86_64-MacOSX.cmake` cache requires that `CMAKE_OSX_DEPLOYMENT_TARGET` is
> set before usage. Therefore, setting `CMAKE_OSX_DEPLOYMENT_TARGET` must come
> before specifying the cache or you will get an error message.
