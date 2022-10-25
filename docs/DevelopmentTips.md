# Development Tips

## Compile times

### Build using Ninja

To *be* a productivity ninja, one must *use* `ninja`. `ninja` can be invoked inside the swift build directory, e.g. `<path>/build/Ninja-ReleaseAssert/swift-macosx-x86_64/`. Running `ninja` (which is equivalent to `ninja all`) will build the local swift, stdlib and overlays. It doesn’t necessarily build all the testing infrastructure, benchmarks, etc.

`ninja -t targets` gives a list of all possible targets to pass to ninja. This is useful for grepping.

For this example, we will figure out how to quickly iterate on a change to the standard library to fix 32-bit build errors while building on a 64-bit host, suppressing warnings along the way.

`ninja -t targets | grep stdlib | grep i386` will output many targets, but at the bottom we see `swift-stdlib-iphonesimulator-i386`, which looks like a good first step. This target will just build i386 parts and not waste our time also building the 64-bit stdlib, overlays, etc.

Going further, ninja can spawn a web browser for you to navigate dependencies and rules. `ninja -t browse swift-stdlib-iphonesimulator-i386`  will open a webpage with hyperlinks for all related targets. “target is built using” lists all this target’s dependencies, while “dependent edges build” list all the targets that depend directly on this.

Clicking around a little bit, we can find `lib/swift/iphonesimulator/i386/libswiftCore.dylib` as a commonly-depended-upon target. This will perform just what is needed to compile the standard library for i386 and nothing else.

Going further, for various reasons the standard library has lots of warnings. This is actively being addressed, but fixing all of them may require language features, etc. In the meantime, let’s suppress warnings in our build so that we just see the errors. `ninja -nv lib/swift/iphonesimulator/i386/libswiftCore.dylib` will show us the actual commands ninja will issue to build the i386 stdlib. (You’ll notice that an incremental build here is merely 3 commands as opposed to ~150 for `swift-stdlib-iphonesimulator-i386`).

Copy the invocation that has  ` -o <build-path>/swift-macosx-x86_64/stdlib/public/core/iphonesimulator/i386/Swift.o`, so that we can perform the actual call to swiftc ourselves. Tack on `-suppress-warnings` at the end, and now we have the command to just build `Swift.o` for i386 while only displaying the actual errors.

### Choosing the bootstrapping mode
By default, the compiler builds with the `bootstrapping-with-hostlibs` (macOS) or `bootstrapping` (Linux) bootstrapping mode. To speed up local development it's recommended to build with the `hosttools` mode: `utils/build-script --bootstrapping=hosttools`.

It requires a recently new swift toolchain to be installed on your build machine. You might need to download and install a nightly Swift toolchain to build the Swift project in `hosttools` mode.

Not that changing the bootstrapping mode needs a reconfiguration.

#### Using a locally built Swift toolchain

If you do not want to install a nightly Swift toolchain, or you need to debug Swift code within SwiftCompilerSources, you can build the Swift toolchain in `bootstrapping-with-hostlibs` mode on your local machine once, and then use this toolchain to iterate on your changes with the `hosttools` mode:

* Build the toolchain locally in `bootstrapping-with-hostlibs` mode: `./utils/build-toolchain com.yourname`.
* Copy the `swift-LOCAL-YYYY-MM-DD.xctoolchain` file from `./swift-nightly-install/Library/Developer/Toolchains` to `/Library/Developer/Toolchains`.
* Launch Xcode, in the menu bar select _Xcode_ > _Toolchains_ > _Local Swift Development Snapshot YYYY-MM-DD_.
* Remove the Swift build directory: `./build`.
* Run the Swift build script with the locally built Swift toolchain in `hosttools` mode: `TOOLCHAINS=com.yourname.YYYYMMDD ./utils/build-script --bootstrapping=hosttools`. Repeat this step as you iterate on your change.

To debug using LLDB, run LLDB from the locally built toolchain with a couple of environment variables set:
```
DYLD_LIBRARY_PATH=/Library/Developer/Toolchains/swift-LOCAL-YYYY-MM-DD.xctoolchain/usr/lib/swift/macosx DYLD_FRAMEWORK_PATH=/Applications/Xcode.app/Contents/Developer/Library/Frameworks /Library/Developer/Toolchains/swift-LOCAL-YYYY-MM-DD.xctoolchain/usr/bin/lldb
```

### Working with two build directories
For developing and debugging you are probably building a debug configuration of swift. But it's often beneficial to also build a release-assert configuration in parallel (`utils/build-script -R`).

The standard library takes very long to build with a debug compiler. It's much faster to build everything (including the standard library) with a release compiler and only the swift-frontend (with `ninja swift-frontend`) in debug. Then copy the release-built standard library to the debug build:
```
src=/path/to/build/Ninja-ReleaseAssert/swift-macosx-x86_64
dst=/path/to/build/Ninja-DebugAssert/swift-macosx-x86_64
cp -r $src/stdlib $dst/
cp -r $src/lib/swift/macosx $dst/lib/swift/
cp -r $src/lib/swift/shims $dst/lib/swift/
```

### Use sccache to cache build artifacts

Compilation times for the compiler and the standard library can be agonizing, especially for cold builds. This is particularly painful if

* You're bisecting an issue over a large period of time.
* You're switching between Xcode versions to debug issues.
* You have multiple distinct work directories, say for working on multiple things at once.

[`sccache`](https://github.com/mozilla/sccache) provides a mostly automatic caching experience for C and C++ build artifacts. Here is how you can set it up and use it on macOS:

```
$ brew install sccache
$ ./swift/utils/build-script MY_ARGS --sccache
```

If you want to always use sccache, you can `export SWIFT_USE_SCCACHE=1` and omit the `--sccache` flag from the `build-script` invocation.

Given the size of artifacts generated, you might also want to bump the cache size from the default 10GB to something larger, say by putting `export SCCACHE_CACHE_SIZE="50G"` in your dotfile(s).  You'll need to restart the `sccache` server after changing that environment variable
(`sccache --stop-server && sccache --start-server`).

You can run some compiles to see if it is actually doing something by running `sccache --show-stats`. Depending on the exact compilation task you're running, you might see very different cache hit rates. For example, `sccache` is particularly effective if you're rebuilding LLVM, which doesn't change so frequently from the Swift compiler's perspective. On the other hand, if you're changing the compiler's AST, the cache hit rate is likely to be much lower.

One known issue with `sccache` is that you might occasionally get an "error: Connection to server timed out", even though you didn't stop the server. Simply re-running the build command usually works.

## Memory usage

When building Swift, peak memory usage happens during the linking phases that produce llvm and swift executables. In case your build fails because a process ran out of RAM, you can use one or more of the following techniques to reduce the peak memory usage.

### Reduce the amount of debug symbols

We can use debug symbols for only the part of the project we intend to work on. For example, when working on the compiler itself, we can build with debug symbols enabled only for the compiler:

```
build-script --release --debug-swift
```

### Reduce the number of parallel link jobs

By default, `build-script` will spawn as many parallel compile / link jobs as there are CPUs in the machine. We can reduce the number of parallel link jobs by setting the `LLVM_PARALLEL_LINK_JOBS` and `SWIFT_PARALLEL_LINK_JOBS` CMake properties. We can set them through the `--llvm-cmake-options` and `--swift-cmake-options` arguments to `build-script`.

For example, to have `build-script` spawn only one link job at a time, we can invoke it as:

```
build-script --llvm-cmake-options=-DLLVM_PARALLEL_LINK_JOBS=1 --swift-cmake-options=-DSWIFT_PARALLEL_LINK_JOBS=1
```

## Using Ninja with Xcode

Although it's possible to build the toolchain entirely with Xcode via `--xcode`,
a more efficient and robust option is to integrate a Ninja build with Xcode.
This is also convenient in that you can navigate, build, run, edit, and debug in
Xcode while retaining the option of using Ninja on the command line.

Assuming that you have already [built the toolchain via Ninja](#the-actual-build),
several more steps are necessary to set up this environment:
* Generate Xcode projects with `utils/build-script --skip-build --xcode --skip-early-swift-driver`.
  This will first build a few LLVM files that are needed to configure the
  projects.
* Create a new Xcode workspace.
* Add the generated Xcode projects or Swift packages that are relevant to your
  tasks to your workspace. All the Xcode projects can be found among the
  build artifacts in `build/Xcode-DebugAssert`. For example:
  * If you are aiming for the compiler, add `build/Xcode-DebugAssert/swift-macosx-*/Swift.xcodeproj`.
    This project also includes the standard library and runtime sources. If you
    need the parts of the compiler that are implemented in Swift itself, add the
    `swift/SwiftCompilerSources/Package.swift` package as well.
  * If you are aiming for just the standard library or runtime, add
    `build/Xcode-DebugAssert/swift-macosx-*/stdlib/Swift-stdlib.xcodeproj`.
  <!-- FIXME: Without this "hard" line break, the note doesn’t get properly spaced from the bullet -->
  <br />

  > **Warning**  
  > Adding both `Swift.xcodeproj` and `LLVM.xcodeproj` *might* slow down the IDE
    and is not recommended unless you know what you're doing.

  In general, we encourage you to add only what you need. Keep in mind that none
  of the generated Xcode projects are required to build or run with this setup
  because we are using Ninja—an *external* build system; rather, they should be
  viewed as a means of leveraging the navigation, editing and debugging features
  of the IDE in relation to the source code they wrap.

* Create an empty Xcode project in the workspace, using the
  _External Build System_ template.
* For a Ninja target that you want to build (e.g. `swift-frontend`), add a
  target to the empty project, using the _External Build System_ template.
* In the _Info_ pane of the target settings, set
  * _Build Tool_ to the path of the `ninja` executable (the output of
    `which ninja` on the command line)
  * _Arguments_ to the Ninja target name (e.g. `swift-frontend`)
  * _Directory_ to the path of the build directory associated with the Ninja
    target. For Swift targets, including the standard library and runtime, you
    want `path/to/swift-project/build/Ninja-*/swift-macosx-*`
* Add a scheme for the target. Be sure not to select a target from one the
  generated Xcode projects.
* > **Note**  
  > Ignore this step if the target associates to a non-executable Ninja target
    like `swift-stdlib`.

  Adjust the _Run_ action settings of the scheme:
  * In the _Info_ pane, select the _Executable_ built by the Ninja target from
    the appropriate `bin` directory (e.g. `build/Ninja-*/swift-macosx-*/bin/swift-frontend`).
  * In the _Arguments_ pane, add the command line arguments that you want to
    pass to the executable on launch (e.g. `path/to/file.swift -typecheck` for
    `swift-frontend`).
  * You can optionally set the working directory for debugging in the
    _Options_ pane.
* Configure as many more target-scheme pairs as you need.

Now you are all set! You can build, run and debug as with a native Xcode
project. If an `update-checkout` routine or a structural change—such as when
source files are added or deleted—happens to impact your editing experience,
simply regenerate the Xcode projects.

> **Note**  
> * For debugging to *fully* work for a given component—say, the compiler—the
    `build-script` invocation for the Ninja build must be arranged to
    [build a debug variant of that component](#debugging-issues).
> * Xcode's indexing can occasionally start slipping after switching to and back
    from a distant branch, resulting in a noticeable slowdown. To sort things
    out, close the workspace and delete the _Index_ directory from the
    workspace's derived data before reopening.
