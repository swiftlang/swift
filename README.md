<img src="https://swift.org/assets/images/swift.svg" alt="Swift logo" height="70" >

# Swift Programming Language

| | **Swift** | **Package** |
|---|:---:|:---:|
|**macOS**         |[![Build Status](https://ci.swift.org/job/oss-swift-incremental-RA-osx/badge/icon)](https://ci.swift.org/job/oss-swift-incremental-RA-osx)|[![Build Status](https://ci.swift.org/job/oss-swift-package-osx/badge/icon)](https://ci.swift.org/job/oss-swift-package-osx)|
|**Ubuntu 14.04** |[![Build Status](https://ci.swift.org/job/oss-swift-incremental-RA-linux-ubuntu-14_04/badge/icon)](https://ci.swift.org/job/oss-swift-incremental-RA-linux-ubuntu-14_04)|[![Build Status](https://ci.swift.org/job/oss-swift-package-linux-ubuntu-14_04/badge/icon)](https://ci.swift.org/job/oss-swift-package-linux-ubuntu-14_04)|
|**Ubuntu 16.04** |[![Build Status](https://ci.swift.org/job/oss-swift-incremental-RA-linux-ubuntu-16_04/badge/icon)](https://ci.swift.org/job/oss-swift-incremental-RA-linux-ubuntu-16_04)|[![Build Status](https://ci.swift.org/job/oss-swift-package-linux-ubuntu-16_04/badge/icon)](https://ci.swift.org/job/oss-swift-package-linux-ubuntu-16_04)|
|**Ubuntu 16.10** |[![Build Status](https://ci.swift.org/job/oss-swift-incremental-RA-linux-ubuntu-16_10/badge/icon)](https://ci.swift.org/job/oss-swift-incremental-RA-linux-ubuntu-16_10)|[![Build Status](https://ci.swift.org/job/oss-swift-package-linux-ubuntu-16_10/badge/icon)](https://ci.swift.org/job/oss-swift-package-linux-ubuntu-16_10)|

**Welcome to Swift!**

Swift is a high-performance system programming language.  It has a clean
and modern syntax, offers seamless access to existing C and Objective-C code
and frameworks, and is memory safe by default.

Although inspired by Objective-C and many other languages, Swift is not itself a
C-derived language. As a complete and independent language, Swift packages core
features like flow control, data structures, and functions, with high-level
constructs like objects, protocols, closures, and generics. Swift embraces
modules, eliminating the need for headers and the code duplication they entail.

## Contributing to Swift

Contributions to Swift are welcomed and encouraged! Please see the
[Contributing to Swift guide](https://swift.org/contributing/).

To be a truly great community, [Swift.org](https://swift.org/) needs to welcome
developers from all walks of life, with different backgrounds, and with a wide
range of experience. A diverse and friendly community will have more great
ideas, more unique perspectives, and produce more great code. We will work
diligently to make the Swift community welcoming to everyone.

To give clarity of what is expected of our members, Swift has adopted the
code of conduct defined by the Contributor Covenant. This document is used
across many open source communities, and we think it articulates our values
well. For more, see the [Code of Conduct](https://swift.org/community/#code-of-conduct).

## Getting Started

These instructions give the most direct path to a working Swiftdevelopment
environment. To build from source you will need 2gb of disk space for the
source code and over 20gb of disk space for the build artifacts. The first
build will take multiple hours, but incremental builds will finish much faster.


### System Requirements

macOS, Ubuntu Linux LTS, and the latest Ubuntu Linux release are the current
supported host development operating systems.

To build for macOS, you always need the latest version of Xcode, currently
[Xcode 9.0 beta 4](https://developer.apple.com/xcode/downloads/). You will
also need [CMake](https://cmake.org) and [Ninja](https://ninja-build.org),
which can be installed via a package manager:

**[Homebrew](https://brew.sh/)**

    brew install cmake ninja

**[MacPorts](https://macports.org)**

    sudo port install cmake ninja

Instructions for installing CMake and Ninja directly can be found [here](#build-dependencies)

#### Linux

<a name="linux-dependencies"></a>For Ubuntu, you'll need the following
development dependencies:

    sudo apt-get install git cmake ninja-build clang python uuid-dev libicu-dev icu-devtools libbsd-dev libedit-dev libxml2-dev libsqlite3-dev swig libpython-dev libncurses5-dev pkg-config libblocksruntime-dev libcurl4-openssl-dev autoconf libtool systemtap-sdt-dev tzdata

**Note:** LLDB currently requires at least `swig-1.3.40` but will successfully build
with version 2 shipped with Ubuntu.

Build instructions for Ubuntu 14.04 LTS can be found [here](docs/Ubuntu14.md)

### Getting Sources for Swift and Related Projects

First create a directory for all of the Swift sources:

    mkdir swift-source
    cd swift-source

**Note:** This is important since update-checkout (see below) checks out
repositories next to the Swift source directory. This means that if one clones
Swift and has other unrelated repositories, update-checkout may not clone those
repositories and will update them instead.

**Via HTTPS**  For those checking out sources as read-only, HTTPS works best:

    git clone https://github.com/apple/swift.git
    ./swift/utils/update-checkout --clone

**Via SSH**  For those who plan on regularly making direct commits,
cloning over SSH may provide a better experience (which requires
[uploading SSH keys to GitHub](https://help.github.com/articles/adding-a-new-ssh-key-to-your-github-account/)):

    git clone git@github.com:apple/swift.git
    ./swift/utils/update-checkout --clone-with-ssh

### Building Swift

The `build-script` is a high-level build automation script that supports basic
options such as building a Swift-compatible LLDB, building the Swift Package
Manager, building for iOS, running tests after builds, and more. It also
supports presets, which you can define for common combinations of build options.

View the inline help to find out more, in particular the section "Typical uses"

    utils/build-script -h

A basic command to build Swift with optimizations and run basic tests with
Ninja:

    utils/build-script -r -t

### Development Environment

There are two primary build options to use with CMake, xcodebuild or Ninja.
xcodebuild integrates nicely with Xcode.app, but Ninja is a bit faster and
supports more environments.

To build using Xcode, run:

    utils/build-script -x

The Xcode IDE can be used to edit the Swift source code, but it is not currently
fully supported as a build environment for SDKs other than macOS. If you need to
work with other SDKs, you'll need to create a second build using Ninja.

To build using Ninja, run:

    utils/build-script -R --debug-swift

This will build LLVM+Clang with optimizations and no debug symbols, but will
build swift with debug symbols. This is a good starting point since the debugger
will work in the swift code base, and the tests will run faster.

#### Build Products

All of the build products are placed in `swift-source/build/${TOOL}-${MODE}/${PRODUCT}-${PLATFORM}/`.
If macOS swift with Ninja in DebugAssert mode was built, all of the products
would be in `swift-source/build/Ninja-DebugAssert/swift-macosx-x86_64/`. It
helps to save this directory as an environment variable for future use.

    export SWIFT_BUILD_ROOT="~/swift-source/build/Ninja-DebugAssert/swift-macosx-x86_64"

#### Ninja

Once the first build has been finished with `build-script`, ninja can perform
fast incremental builds of various products.

    cd ${SWIFT_BUILD_ROOT}
    ninja swift
    ninja swift-stdlib

These incremental builds are a big timesaver when developing and debugging. It
is still always a good idea to do a full build after using `update-checkout`.
Building the `swift-stdlib` target as an additional layer of testing from time
to time is also a good idea.

#### Xcode

To open the swift in Xcode, open `${SWIFT_BUILD_ROOT}/Swift.xcodeproj`. It will
auto-create a *lot* of schemes for all of the available targets. A common debug
flow would involve:

 - Select the ‘swift’ scheme
 - Pull up the scheme editor (⌘+⇧+<)
 - Select the ‘Arguments’ tab and click the ‘+’
 - Add the command line options
 - Close the scheme editor
 - Build and run

### Build Failures

Make sure you are using the latest release of Xcode to build, including Betas.
If you have changed Xcode versions but still encounter errors that appear to
be related to the Xcode version, try passing `-- --rebuild` to `build-script`.

Make sure all repositories are up to date with the `update-checkout` command
described above.

## Testing Swift

See [docs/Testing.md](docs/Testing.md).

## Documentation

To read the documentation, start by installing the
[Sphinx](http://sphinx-doc.org) documentation generator tool by running the command:

`easy_install -U Sphinx`

Once complete, you can build the Swift documentation by changing directory into
[docs](https://github.com/apple/swift/tree/master/docs) and typing `make`. This
compiles the `.rst` files in the [docs](https://github.com/apple/swift/tree/master/docs)
directory into HTML in the `docs/_build/html` directory.

Many of the docs are out of date, but you can see some historical design
documents in the `docs` directory.

Another source of documentation is the standard library itself, located in
`stdlib`. Much of the language is actually implemented in the library
(including `Int`), and the standard library gives some examples of what can be
expressed today.

## Build Dependencies

### CMake
[CMake](https://cmake.org) is the core infrastructure used to configure builds of 
Swift and its companion projects; at least version 3.4.3 is required. 

On macOS, you can download the [CMake Binary Distribution](https://cmake.org/download),
bundled as an application, copy it to `/Applications`, and add the embedded
command line tools to your `PATH`:

    export PATH=/Applications/CMake.app/Contents/bin:$PATH

On Linux, if you have not already installed Swift's [development
dependencies](#linux-dependencies), you can download and install the CMake
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
