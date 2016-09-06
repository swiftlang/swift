usage: 
  build-script [-h | --help] [OPTION ...]
  build-script --preset=NAME [SUBSTITUTION ...]

Use this tool to build, test, and prepare binary distribution archives of Swift
and related tools.

Builds Swift (and, optionally, LLDB), incrementally, optionally
testing it thereafter.  Different build configurations are maintained in
parallel.

optional arguments:
  -h, --help            show this help message and exit
  -n, --dry-run         print the commands that would be executed, but do not
                        execute them
  --no-legacy-impl      avoid legacy implementation
  -d, --debug           build the Debug variant of everything (LLVM, Clang,
                        Swift host tools, target Swift standard libraries,
                        LLDB (if enabled) (default)
  -r, --release-debuginfo
                        build the RelWithDebInfo variant of everything
                        (default is Debug)
  -R, --release         build the Release variant of everything (default is
                        Debug)
  --assertions          enable assertions in all projects
  --no-assertions       disable assertions in all projects
  --build-runtime-with-host-compiler [BOOL]
                        Use the host compiler, not the self-built one to
                        compile the Swift runtime
  -i, --ios             also build for iOS, but disallow tests that require an
                        iOS device
  --skip-ios            set to skip everything iOS-related
  --tvos [BOOL]         also build for tvOS, but disallow tests that require a
                        tvos device
  --skip-tvos           set to skip everything tvOS-related
  --watchos [BOOL]      also build for watchOS, but disallow tests that
                        require an watchOS device
  --skip-watchos        set to skip everything watchOS-related
  --android [BOOL]      also build for Android
  --swift-analyze-code-coverage {false,not-merged,merged}
                        enable code coverage analysis in Swift (false, not-
                        merged, merged).
  --build-subdir PATH   name of the directory under $SWIFT_BUILD_ROOT where
                        the build products will be placed
  --install-prefix PATH
                        The installation prefix. This is where built Swift
                        products (like bin, lib, and include) will be
                        installed.
  --install-symroot PATH
                        the path to install debug symbols into
  -j BUILD_JOBS, --jobs BUILD_JOBS
                        the number of parallel build jobs to use
  --darwin-xcrun-toolchain DARWIN_XCRUN_TOOLCHAIN
                        the name of the toolchain to use on Darwin
  --cmake PATH          the path to a CMake executable that will be used to
                        build Swift
  --show-sdks [BOOL]    print installed Xcode and SDK versions
  --extra-swift-args EXTRA_SWIFT_ARGS
                        Pass through extra flags to swift in the form of a
                        cmake list 'module_regexp;flag'. Can be called
                        multiple times to add multiple such module_regexp flag
                        pairs. All semicolons in flags must be escaped with a
                        '\'
  --host-cc PATH        the absolute path to CC, the 'clang' compiler for the
                        host platform. Default is auto detected.
  --host-cxx PATH       the absolute path to CXX, the 'clang++' compiler for
                        the host platform. Default is auto detected.
  --distcc [BOOL]       use distcc in pump mode
  --enable-asan [BOOL]  enable Address Sanitizer
  --enable-ubsan [BOOL]
                        enable Undefined Behavior Sanitizer
  --clang-compiler-version MAJOR.MINOR.PATCH
                        string that indicates a compiler version for Clang
  --darwin-deployment-version-osx MAJOR.MINOR
                        minimum deployment target version for OS X
  --darwin-deployment-version-ios MAJOR.MINOR
                        minimum deployment target version for iOS
  --darwin-deployment-version-tvos MAJOR.MINOR
                        minimum deployment target version for tvOS
  --darwin-deployment-version-watchos MAJOR.MINOR
                        minimum deployment target version for watchOS
  --extra-cmake-options EXTRA_CMAKE_OPTIONS
                        Pass through extra options to CMake in the form of
                        comma separated options
                        '-DCMAKE_VAR1=YES,-DCMAKE_VAR2=/tmp'. Can be called
                        multiple times to add multiple such options.
  --build-args BUILD_ARGS
                        arguments to the build tool. This would be prepended
                        to the default argument that is '-j8' when CMake
                        generator is "Ninja".
  --verbose-build [BOOL]
                        print the commands executed during the build
  --lto [LTO_TYPE]      use lto optimization on llvm/swift tools. This does
                        not imply using lto on the swift standard library or
                        runtime. Options: thin, full. If no optional arg is
                        provided, full is chosen by default
  --llvm-max-parallel-lto-link-jobs COUNT
                        the maximum number of parallel link jobs to use when
                        compiling llvm
  --swift-tools-max-parallel-lto-link-jobs COUNT
                        the maximum number of parallel link jobs to use when
                        compiling swift tools.
  --build-jobs [BUILD_JOBS], --common-cmake-options [BUILD_JOBS], --only-execute [BUILD_JOBS], --skip-test-optimized [BUILD_JOBS]
  --lit-args LITARGS    lit args to use when testing

Host and cross-compilation targets:
  --host-target HOST_TARGET
                        The host target. LLVM, Clang, and Swift will be built
                        for this target. The built LLVM and Clang will be used
                        to compile Swift for the cross-compilation targets.
  --cross-compile-hosts CROSS_COMPILE_HOSTS
                        A space separated list of targets to cross-compile
                        host Swift tools for. Can be used multiple times.
  --stdlib-deployment-targets STDLIB_DEPLOYMENT_TARGETS
                        list of targets to compile or cross-compile the Swift
                        standard library for. None by default.
  --build-stdlib-deployment-targets BUILD_STDLIB_DEPLOYMENT_TARGETS
                        A space-separated list that filters which of the
                        configured targets to build the Swift standard library
                        for, or 'all'.

Options to select projects:
  -l, --lldb            build LLDB
  -b, --llbuild         build llbuild
  -p, --swiftpm         build swiftpm
  --xctest [BOOL]       build xctest
  --foundation [BOOL]   build foundation
  --libdispatch [BOOL]  build libdispatch
  --playgroundlogger    build playgroundlogger
  --playgroundsupport   build PlaygroundSupport
  --build-ninja [BOOL]  build the Ninja tool

Extra actions to perform before or in addition to building:
  -c, --clean           do a clean build
  --export-compile-commands [BOOL]
                        generate compilation databases in addition to building
  --symbols-package PATH
                        if provided, an archive of the symbols directory will
                        be generated at this path

Override build variant for a specific project:
  --debug-llvm          build the Debug variant of LLVM
  --debug-swift         build the Debug variant of Swift host tools
  --debug-swift-stdlib  build the Debug variant of the Swift standard library
                        and SDK overlay
  --debug-lldb          build the Debug variant of LLDB
  --debug-cmark         build the Debug variant of CommonMark
  --debug-foundation    build the Debug variant of Foundation
  --debug-libdispatch   build the Debug variant of libdispatch

Control assertions in a specific project:
  --cmark-assertions    enable assertions in CommonMark
  --llvm-assertions     enable assertions in LLVM
  --no-llvm-assertions  disable assertions in LLVM
  --swift-assertions    enable assertions in Swift
  --no-swift-assertions
                        disable assertions in Swift
  --swift-stdlib-assertions
                        enable assertions in the Swift standard library
  --no-swift-stdlib-assertions
                        disable assertions in the Swift standard library
  --lldb-assertions     enable assertions in LLDB
  --no-lldb-assertions  disable assertions in LLDB

Select the CMake generator:
  -x, --xcode           use CMake's Xcode generator (default is Ninja)
  -m, --make            use CMake's Makefile generator (default is Ninja)
  -e, --eclipse         use CMake's Eclipse generator (default is Ninja)

Run tests:
  -t                    test Swift after building
  --test [BOOL]         test Swift after building
  -T                    run the validation test suite (implies --test)
  --validation-test [BOOL]
                        run the validation test suite (implies --test)
  -o                    run the test suite in optimized mode too (implies
                        --test)
  --test-optimized [BOOL]
                        run the test suite in optimized mode too (implies
                        --test)
  --long-test [BOOL]    run the long test suite
  --host-test [BOOL]    run executable tests on host devices (such as iOS or
                        tvOS)
  -B, --benchmark       run the Swift Benchmark Suite after building
  --skip-test-osx [BOOL]
                        skip testing Swift stdlibs for Mac OS X
  --skip-test-linux [BOOL]
                        skip testing Swift stdlibs for Linux
  --skip-test-freebsd [BOOL]
                        skip testing Swift stdlibs for FreeBSD
  --skip-test-cygwin [BOOL]
                        skip testing Swift stdlibs for Cygwin

Run build:
  --build-swift-dynamic-stdlib [BOOL]
                        build dynamic variants of the Swift standard library
  --build-swift-static-stdlib [BOOL]
                        build static variants of the Swift standard library
  --build-swift-dynamic-sdk-overlay [BOOL]
                        build dynamic variants of the Swift SDK overlay
  --build-swift-static-sdk-overlay [BOOL]
                        build static variants of the Swift SDK overlay
  --build-swift-stdlib-unittest-extra [BOOL]
                        Build optional StdlibUnittest components
  -S, --skip-build      generate build directory only without building
  --skip-build-linux [BOOL]
                        skip building Swift stdlibs for Linux
  --skip-build-freebsd [BOOL]
                        skip building Swift stdlibs for FreeBSD
  --skip-build-cygwin [BOOL]
                        skip building Swift stdlibs for Cygwin
  --skip-build-osx [BOOL]
                        skip building Swift stdlibs for MacOSX
  --skip-build-ios [BOOL]
                        skip building Swift stdlibs for iOS
  --skip-build-ios-device [BOOL]
                        skip building Swift stdlibs for iOS devices (i.e.
                        build simulators only)
  --skip-build-ios-simulator [BOOL]
                        skip building Swift stdlibs for iOS simulator (i.e.
                        build devices only)
  --skip-build-tvos [BOOL]
                        skip building Swift stdlibs for tvOS
  --skip-build-tvos-device [BOOL]
                        skip building Swift stdlibs for tvOS devices (i.e.
                        build simulators only)
  --skip-build-tvos-simulator [BOOL]
                        skip building Swift stdlibs for tvOS simulator (i.e.
                        build devices only)
  --skip-build-watchos [BOOL]
                        skip building Swift stdlibs for watchOS
  --skip-build-watchos-device [BOOL]
                        skip building Swift stdlibs for watchOS devices (i.e.
                        build simulators only)
  --skip-build-watchos-simulator [BOOL]
                        skip building Swift stdlibs for watchOS simulator
                        (i.e. build devices only)
  --skip-build-android [BOOL]
                        skip building Swift stdlibs for Android
  --skip-build-benchmarks [BOOL]
                        skip building Swift Benchmark Suite

Skip testing specified targets:
  --skip-test-ios [BOOL]
                        skip testing all iOS targets. Equivalent to specifying
                        both --skip-test-ios-simulator and --skip-test-ios-
                        host
  --skip-test-ios-simulator [BOOL]
                        skip testing iOS simulator targets
  --skip-test-ios-host [BOOL]
                        skip testing iOS device targets on the host machine
                        (the phone itself)
  --skip-test-tvos [BOOL]
                        skip testing all tvOS targets. Equivalent to
                        specifying both --skip-test-tvos-simulator and --skip-
                        test-tvos-host
  --skip-test-tvos-simulator [BOOL]
                        skip testing tvOS simulator targets
  --skip-test-tvos-host [BOOL]
                        skip testing tvOS device targets on the host machine
                        (the TV itself)
  --skip-test-watchos [BOOL]
                        skip testing all tvOS targets. Equivalent to
                        specifying both --skip-test-watchos-simulator and
                        --skip-test-watchos-host
  --skip-test-watchos-simulator [BOOL]
                        skip testing watchOS simulator targets
  --skip-test-watchos-host [BOOL]
                        skip testing watchOS device targets on the host
                        machine (the watch itself)
  --skip-test-android-host [BOOL]
                        skip testing Android device targets on the host
                        machine (the phone itself)

Build settings for Android:
  --android-ndk PATH    An absolute path to the NDK that will be used as a
                        libc implementation for Android builds
  --android-api-level ANDROID_API_LEVEL
                        The Android API level to target when building for
                        Android. Currently only 21 or above is supported
  --android-ndk-gcc-version {4.8,4.9}
                        The GCC version to use when building for Android.
                        Currently only 4.9 is supported. 4.9 is also the
                        default value. This option may be used when
                        experimenting with versions of the Android NDK not
                        officially supported by Swift
  --android-icu-uc PATH
                        Path to a directory containing libicuuc.so
  --android-icu-uc-include PATH
                        Path to a directory containing headers for libicuuc
  --android-icu-i18n PATH
                        Path to a directory containing libicui18n.so
  --android-icu-i18n-include PATH
                        Path to a directory containing headers libicui18n
  --android-deploy-device-path PATH
                        Path on an Android device to which built Swift stdlib
                        products will be deployed. If running host tests,
                        specify the '/data/local/tmp' directory.

Using option presets:

  --preset-file=PATH    load presets from the specified file

  --preset=NAME         use the specified option preset

  The preset mode is mutually exclusive with other options.  It is not
  possible to add ad-hoc customizations to a preset.  This is a deliberate
  design decision.  (Rationale: a preset is a certain important set of
  options that we want to keep in a centralized location.  If you need to
  customize it, you should create another preset in a centralized location,
  rather than scattering the knowledge about the build across the system.)

  Presets support substitutions for controlled customizations.  Substitutions
  are defined in the preset file.  Values for substitutions are supplied
  using the name=value syntax on the command line.

Any arguments not listed are forwarded directly to Swift's
'build-script-impl'.  See that script's help for details.

Environment variables
---------------------

This script respects a few environment variables, should you
choose to set them:

SWIFT_SOURCE_ROOT: a directory containing the source for LLVM, Clang, Swift.
                   If this script is located in a Swift
                   source directory, the location of SWIFT_SOURCE_ROOT will be
                   inferred if the variable is not set.

'build-script' expects the sources to be laid out in the following way:

   $SWIFT_SOURCE_ROOT/llvm
                     /clang
                     /swift
                     /lldb                       (optional)
                     /llbuild                    (optional)
                     /swiftpm                    (optional, requires llbuild)
                     /compiler-rt                (optional)
                     /swift-corelibs-xctest      (optional)
                     /swift-corelibs-foundation  (optional)
                     /swift-corelibs-libdispatch (optional)

SWIFT_BUILD_ROOT: a directory in which to create out-of-tree builds.
                  Defaults to "$SWIFT_SOURCE_ROOT/build/".

Preparing to run this script
----------------------------

  See README.md for instructions on cloning Swift subprojects.

If you intend to use the -l, -L, --lldb, or --lldb-debug options.

That's it; you're ready to go!

Examples
--------

Given the above layout of sources, the simplest invocation of 'build-script' is
just:

  [~/src/s]$ ./swift/utils/build-script

This builds LLVM, Clang, Swift and Swift standard library in debug mode.

All builds are incremental.  To incrementally build changed files, repeat the
same 'build-script' command.

Typical uses of 'build-script'
------------------------------

To build everything with optimization without debug information:

  [~/src/s]$ ./swift/utils/build-script -R

To run tests, add '-t':

  [~/src/s]$ ./swift/utils/build-script -R -t

To run normal tests and validation tests, add '-T':

  [~/src/s]$ ./swift/utils/build-script -R -T

To build LLVM+Clang with optimization without debug information, and a
debuggable Swift compiler:

  [~/src/s]$ ./swift/utils/build-script -R --debug-swift

To build a debuggable Swift standard library:

  [~/src/s]$ ./swift/utils/build-script -R --debug-swift-stdlib

iOS build targets are always configured and present, but are not built by
default.  To build the standard library for OS X, iOS simulator and iOS device:

  [~/src/s]$ ./swift/utils/build-script -R -i

To run OS X and iOS tests that don't require a device:

  [~/src/s]$ ./swift/utils/build-script -R -i -t

To use 'make' instead of 'ninja', use '-m':

  [~/src/s]$ ./swift/utils/build-script -m -R

To create Xcode projects that can build Swift, use '-x':

  [~/src/s]$ ./swift/utils/build-script -x -R

Preset mode in build-script
---------------------------

All buildbots and automated environments use 'build-script' in *preset mode*.
In preset mode, the command line only specifies the preset name and allows
limited customization (extra output paths).  The actual options come from
the selected preset in 'utils/build-presets.ini'.  For example, to build like
the incremental buildbot, run:

  [~/src/s]$ ./swift/utils/build-script --preset=buildbot_incremental

To build with AddressSanitizer:

  [~/src/s]$ ./swift/utils/build-script --preset=asan

To build a root for Xcode XYZ, '/tmp/xcode-xyz-root.tar.gz':

  [~/src/s]$ ./swift/utils/build-script --preset=buildbot_BNI_internal_XYZ \
      install_destdir="/tmp/install"
      install_symroot="/tmp/symroot"
      installable_package="/tmp/xcode-xyz-root.tar.gz"

If you have your own favorite set of options, you can create your own, local,
preset.  For example, let's create a preset called 'ds' (which stands for
Debug Swift):

  $ cat > ~/.swift-build-presets
  [preset: ds]
  release
  debug-swift
  debug-swift-stdlib
  test
  build-subdir=ds

To use it, specify the '--preset=' argument:

  [~/src/s]$ ./swift/utils/build-script --preset=ds
  ./swift/utils/build-script: using preset 'ds', which expands to
  ./swift/utils/build-script --release --debug-swift --debug-swift-stdlib      --test
  --build-subdir=ds --
  ...

Philosophy
----------

While you can invoke CMake directly to build Swift, this tool will save you
time by taking away the mechanical parts of the process, providing you controls
for the important options.

For all automated build environments, this tool is regarded as *the* *only* way
to build Swift.  This is not a technical limitation of the Swift build system.
It is a policy decision aimed at making the builds uniform across all
environments and easily reproducible by engineers who are not familiar with the
details of the setups of other systems or automated environments.
