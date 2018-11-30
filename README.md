# Swift for TensorFlow

| OS | CI platform | x86_64 | GPU |
|---|:---:|:---:|:---:|
| **macOS** | Google Kokoro | ![Build Status](https://storage.googleapis.com/tensorflow-kokoro-build-badges/macos-swift-tf-release.svg) | _coming soon_ |
| **Ubuntu 16.04** | Swift.org CI | [![Build Status](https://ci-external.swift.org/job/oss-swift-RA-linux-ubuntu-16.04-tensorflow/lastCompletedBuild/badge/icon)](https://ci-external.swift.org/job/oss-swift-RA-linux-ubuntu-16.04-tensorflow) | N/A |

Welcome to Swift for TensorFlow!

Swift for TensorFlow is a new programming language for TensorFlow. It is a copy of the compiler for the [Swift Programming Language](https://swift.org) that adds first-class compiler and language support for machine learning.

This repository covers the compiler and standard libraries. Please visit the [documentation repository](https://github.com/tensorflow/swift) for more information about the project, including a project overview, technical details, and guidelines for contributing. To use Swift for TensorFlow out of the box, follow the [installation instructions](https://github.com/tensorflow/swift/blob/master/Installation.md). To build from source, follow the instructions below.

**Note:** Swift for TensorFlow is an early stage research project. It has been released to enable open source development and is not yet ready for general use by machine learning developers.

## Building Swift for TensorFlow

These instructions give the most direct path to a working development environment for Swift for TensorFlow.

**Note:** Building from source is necessary only if you want to modify the source code or build with a custom version of TensorFlow.

To build from source you will need 2 GB of disk space for the source code and over 20 GB of disk space for the build artifacts. A clean build can take multiple hours, but incremental builds will finish much faster.

### System Requirements

macOS, Ubuntu Linux LTS 14.04, and Ubuntu Linux LTS 16.04 are the current
supported host development operating systems.

#### macOS

To build for macOS, you need [Xcode 10.0](https://developer.apple.com/xcode/downloads/).
The required version of Xcode changes frequently, and is often a beta release.
Check this document for the current required version.

You will also need [CMake](https://cmake.org), [Ninja](https://ninja-build.org), and [Bazel](https://www.bazel.build), which can be installed via a package manager.

**[Homebrew](https://brew.sh/)**

```shell
brew install cmake ninja
brew cask install caskroom/versions/java8 # required for Bazel
brew install bazel # required for TensorFlow support
```

Instructions for installing CMake, Ninja, and Bazel directly can be found [below](#build-dependencies).

#### Linux

For Ubuntu, you'll need the following development dependencies:

    sudo apt-get install git cmake ninja-build clang python uuid-dev libicu-dev icu-devtools libbsd-dev libedit-dev libxml2-dev libsqlite3-dev swig libpython-dev libncurses5-dev pkg-config libblocksruntime-dev libcurl4-openssl-dev systemtap-sdt-dev tzdata rsync

Additionally, [Bazel](https://www.bazel.build) is required to build with TensorFlow support. Ubuntu installation instructions are [here](https://docs.bazel.build/versions/master/install-ubuntu.html).

**Note:** LLDB currently requires at least `swig-1.3.40` but will successfully build
with version 2 shipped with Ubuntu.

Additional build instructions for Ubuntu 14.04 LTS can be found [here](docs/Ubuntu14.md). These are necessary for building Swift for TensorFlow correctly.

### Getting Sources for Swift and Related Projects

First, create a directory for all of the Swift sources:

    mkdir swift-source
    cd swift-source

**Note:** This is important since update-checkout (see below) checks out
repositories next to the Swift source directory. This means that if one clones
Swift and has other unrelated repositories, update-checkout may not clone those
repositories and will update them instead.

**TensorFlow Support:** To build with TensorFlow support, the `tensorflow`
scheme must be specified when cloning sources. The `tensorflow` scheme pins
specific versions of every Swift companion directory and is updated with every
upstream merge from the master branch.

**Via HTTPS**  For those checking out sources as read-only, HTTPS works best:

    git clone https://github.com/apple/swift.git -b tensorflow
    ./swift/utils/update-checkout --clone --scheme tensorflow
    cd swift

**Via SSH**  For those who plan on regularly making direct commits,
cloning over SSH may provide a better experience (which requires
[uploading SSH keys to GitHub](https://help.github.com/articles/adding-a-new-ssh-key-to-your-github-account/)):

    git clone git@github.com:apple/swift.git -b tensorflow
    ./swift/utils/update-checkout --clone-with-ssh --scheme tensorflow
    cd swift

### Building Swift with TensorFlow support

The `build-script` is a high-level build automation script that supports basic
options such as building a Swift-compatible LLDB, building the Swift Package
Manager, building for various platforms, running tests after builds, and more.
TensorFlow support is enabled by the `--enable-tensorflow` flag. TensorFlow will
be automatically cloned from GitHub and built from source using Bazel when this
flag is specified.

There are two primary build systems to use: Xcode and Ninja. The Xcode build
system allows you to work in Xcode, but Ninja is a bit faster and supports
more environments.

To build using Ninja, run:

    utils/build-script --enable-tensorflow --release-debuginfo

When developing Swift, it helps to build what you're working on in a debug
configuration while building the rest of the project with optimizations. Below
are some examples of using debug variants:

    utils/build-script --enable-tensorflow --release-debuginfo --debug-swift # Swift frontend built in debug
    utils/build-script --enable-tensorflow --release-debuginfo --debug-swift-stdlib # Standard library built in debug
    utils/build-script --enable-tensorflow --release-debuginfo --debug-swift --force-optimized-typechecker # Swift frontend sans type checker built in debug

Limiting the amount of debug code in the compiler has a very large impact on
Swift compile times, and in turn the test execution time. If you want to build
the entire project in debug, you can run:

    utils/build-script  --enable-tensorflow --debug

For documentation of all available arguments, as well as additional usage
information, see the inline help:

    utils/build-script -h

### Customize TensorFlow support

 If you want to build with custom TensorFlow headers and shared libraries, please specify the `--tensorflow-host-include-dir` and `--tensorflow-host-lib-dir` arguments:

    utils/build-script --enable-tensorflow --tensorflow-host-include-dir=<path_to_tensorflow_headers> --tensorflow-host-lib-dir=<path_to_tensorflow_libraries>

You can assign specific values to these arguments after a double-dash `--` in
your build-script command. For example:

    utils/build-script -- enable-tensorflow=True

Below is more information about TensorFlow-related build arguments.

* `enable-tensorflow`: If true, enables TensorFlow support for Swift.
    * Default value: `False`.
* `build-tensorflow`: If true, automatically clone and build TensorFlow from source.
    * Default value: If `enable-tensorflow` is `True` and `tensorflow-host-lib-dir` and `tensorflow-host-include-dir` are not specified, then `True`. Otherwise, `False`.
* `host-bazel`: The absolute path to Bazel, used to build TensorFlow.
    * By default, the path is auto detected.
* `tensorflow-bazel-options`: Comma separated options passed to Bazel when building TensorFlow, e.g. `--copt=-mavx,--copt=-msse4.2`.
    * Default: None.
* `tensorflow-host-include-dir`: A directory containing custom TensorFlow headers.
    * Default value: None.
* `tensorflow-host-lib-dir`: A directory containing custom TensorFlow shared libraries (`libtensorflow.so`).
    * Default value: None.
* `tensorflow-swift-bindings`: A generated TensorFlow Swift bindings file (`RawOpsGenerated.swift`) obtained from [tensorflow/swift-bindings](https://github.com/tensorflow/swift-bindings).
    * Default value: `tensorflow-swift-bindings/RawOpsGenerated.swift` if the [tensorflow/swift-bindings](https://github.com/tensorflow/swift-bindings) repository is cloned. Otherwise, none.

### Build systems

#### Xcode

To build using Xcode, specify the `--xcode` argument on any of the above commands.
Xcode can be used to edit the Swift source code, but it is not currently
fully supported as a build environment for SDKs other than macOS. The generated
Xcode project does not integrate with the test runner, but the tests can be run
with the 'check-swift' target.

#### Build Products

All of the build products are placed in `swift-source/build/${TOOL}-${MODE}/${PRODUCT}-${PLATFORM}/`.
If macOS Swift with Ninja in DebugAssert mode was built, all of the products
would be in `swift-source/build/Ninja-DebugAssert/swift-macosx-x86_64/`. It
helps to save this directory as an environment variable for future use.

    export SWIFT_BUILD_DIR="~/swift-source/build/Ninja-DebugAssert/swift-macosx-x86_64"

#### Ninja

Once the first build has completed, Ninja can perform fast incremental builds of
various products. These incremental builds are a big timesaver when developing
and debugging.

    cd ${SWIFT_BUILD_DIR}
    ninja swift

This will build the Swift compiler, but will not rebuild the standard library or
any other target. Building the `swift-stdlib` target as an additional layer of
testing from time to time is also a good idea. To build just the standard
library, run:

    ninja swift-stdlib

It is always a good idea to do a full build after using `update-checkout`.

#### Using Xcode

To open the Swift project in Xcode, open `${SWIFT_BUILD_DIR}/Swift.xcodeproj`.
It will auto-create a *lot* of schemes for all of the available targets. A
common debug flow would involve:

 - Select the 'swift' scheme.
 - Pull up the scheme editor (⌘⇧<).
 - Select the 'Arguments' tab and click the '+'.
 - Add the command line options.
 - Close the scheme editor.
 - Build and run.

Another option is to change the scheme to "Wait for executable to be launched",
then run the build product in Terminal.

### Swift Toolchains

#### Building

Swift toolchains are created using the script
[build-toolchain](https://github.com/apple/swift/blob/master/utils/build-toolchain). This
script is used by swift.org's CI to produce snapshots and can allow for one to
locally reproduce such builds for development or distribution purposes. E.x.:

```
  $ ./utils/build-toolchain $TOOLCHAIN_PREFIX
```

where ``$TOOLCHAIN_PREFIX`` is a string that will be prepended to the swift
package name in the produced tar ball. For instance, if ``$TOOLCHAIN_PREFIX``
was ``macOS``, the produced archive will have the name
``swift-macOS.tar.gz``.

Beyond building the toolchain, ``build-toolchain`` also supports the following
(non-exhaustive) set of useful options::

- ``--dry-run``: Perform a dry run build. This is off by default.
- ``--test``: Test the toolchain after it has been compiled. This is off by default.
- ``--distcc``: Use distcc to speed up the build by distributing the c++ part of
  the swift build. This is off by default.

More options may be added over time. Please pass ``--help`` to
``build-toolchain`` to see the full set of options.

#### Installing into Xcode

On macOS if one wants to install such a toolchain into Xcode:

1. Untar and copy the toolchain to one of `/Library/Developer/Toolchains/` or
   `~/Library/Developer/Toolchains/`. E.x.:

```
  $ tar -xzf swift-macOS.tar.gz -C /
  $ tar -xzf swift-macOS.tar.gz -C ~/
```

2. Specify the local toolchain for Xcode's use via `Xcode->Toolchains`.

### Build Failures

Make sure you are using the [correct release](#macos) of Xcode.

If you have changed Xcode versions but still encounter errors that appear to
be related to the Xcode version, try passing `--rebuild` to `build-script`.

When a new version of Xcode is released, you can update your build without
recompiling the entire project by passing the `--reconfigure` option.

Make sure all repositories are up to date with the `update-checkout` command
described above.

## Testing Swift

The simplest way to run the Swift test suite is using the `tensorflow_test`
build preset, which runs the entire Swift test suite (including new TensorFlow
tests):

    utils/build-script --preset=tensorflow_test

Swift for TensorFlow adds the following new test suites:

- [test/AutoDiff](test/AutoDiff): tests for
  [automatic differentiation](https://github.com/tensorflow/swift/blob/master/docs/AutomaticDifferentiation.md).
- [test/TensorFlow](test/TensorFlow): TensorFlow infrastructure tests that don't
  depend on the TensorFlow runtime.
- [test/TensorFlowRuntime](test/TensorFlowRuntime): TensorFlow runtime tests.

Before submitting pull requests involving large code changes, please run the
command above locally to ensure all tests pass.

For more details on testing, see [docs/Testing.md](docs/Testing.md), in
particular the section on [lit.py](docs/Testing.md#using-litpy).

## Build Dependencies

### CMake
[CMake](https://cmake.org) is the core infrastructure used to configure builds of
Swift and its companion projects; at least version 3.4.3 is required.

On macOS, you can download the [CMake Binary Distribution](https://cmake.org/download),
bundled as an application, copy it to `/Applications`, and add the embedded
command line tools to your `PATH`:

    export PATH=/Applications/CMake.app/Contents/bin:$PATH

On Linux, if you have not already installed Swift's [development
dependencies](#linux), you can download and install the CMake
package separately using the following command:

    sudo apt-get install cmake


### Ninja
[Ninja](https://ninja-build.org) is the current recommended build system
for building Swift and is the default configuration generated by CMake. [Pre-built
packages](https://github.com/ninja-build/ninja/wiki/Pre-built-Ninja-packages)
are available for macOS and Linux distributions. You can also clone Ninja
next to the other projects and it will be bootstrapped automatically:

**Via HTTPS**

    git clone https://github.com/ninja-build/ninja.git && cd ninja
    git checkout release
    cat README

**Via SSH**

    git clone git@github.com:ninja-build/ninja.git && cd ninja
    git checkout release
    cat README

### Bazel
[Bazel](https://bazel.build) is the build tool used to build TensorFlow. Installing Bazel is necessary for building Swift with TensorFlow support.

The Bazel website has detailed installation instructions for
[macOS](https://docs.bazel.build/versions/master/install-os-x.html) and
[Ubuntu](https://docs.bazel.build/versions/master/install-ubuntu.html).
