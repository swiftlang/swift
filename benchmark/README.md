
# Swift Benchmark Suite

This directory contains the Swift Benchmark Suite. This is meant to be a suite
of regression, micro, small, and medium sized benchmarks that are used to
validate the performance of the Swift compiler and standard library.

## Building Swift Benchmarks

There are 3 ways to build the benchmark suite:

1. In tree using ``build-script`` and ``cmake``.
2. Standalone using SwiftPM.
3. Standalone using ``cmake``.

**NOTE:** There are various level of support here. Specifically:

1. Linux is only supported by the standalone ``cmake`` build.
2. Non-macOS Darwin platforms do not support the SwiftPM build.
3. macOS supports all 3 ways of building.

This is not due to intention, but rather, more engineering time would be
required to productize these builds.

### In tree using ``build-script`` and ``cmake``

By default, Swift benchmarks on macOS are compiled in tree during the Swift
build process, e.g.:

    $ swift/utils/build-script --release --no-assertions

To build Swift benchmarks for additional platforms, pass the following flags:

    $ swift/utils/build-script --release --no-assertions --ios --watchos --tvos

macOS benchmark driver binaries are placed in ``bin`` alongside ``swiftc``.
Additional platform binaries are placed in the ``benchmark/bin`` build
directory.

**NOTE:** We assume that the user has installed all Swift project dependencies
since we will build the rest of the compiler as well.

### Building standalone with SwiftPM

To build the benchmark suite with SwiftPM just:

    $ cd ./swift/benchmark
    $ swift build

This can be used to generate an Xcode project that can be used to edit the
benchmarks:

    $ cd ./swift/benchmark
    $ swift package generate-xcodeproj
    $ open swiftbench.xcodeproj

**NOTE:** By default the Xcode project's scheme for SwiftBench will build the
benchmark in debug. One must change it to build the benchmark suite in release
mode.

**NOTE:** To add a new benchmark, one can not add the benchmark via Xcode itself
since we generate the Xcode project from the main ``Package.swift`` in
``./swift/benchmark``. For information on how to do this, please see [Adding New
Benchmarks](#adding-new-benchmarks).

**NOTE:** This is the most convenient way to get up and running/adding
benchmarks quickly when used in combination with a recent nightly [developer
snapshot](https://swift.org/download/#snapshots).  Just use the SwiftPM provided
by the nightly snapshot and everything should work.

### Building standalone with ``cmake``.

To build the Swift benchmarks using only an Xcode installation: install an
Xcode version with Swift support, install ``cmake`` 2.8.12, and ensure Xcode is
selected with ``xcode-select``.

There are two ways to build this. The first is against a specific swift
snapshot:

    $ cmake -G Ninja \
        -DSWIFT_EXEC=$PATH_TO_SWIFTC \
        -DSWIFT_DARWIN_XCRUN_TOOLCHAIN=XcodeDefault \
        ./swift/benchmark

This will configure the benchmarks to build using the specified ``swiftc``, but
use the ``clang``/SDK from the XcodeDefault toolchain of the installed cmake.

The second involves specifying by hand all of the paths that ``cmake`` needs rather
than relying on inference via ``SWIFT_DARWIN_XCRUN_TOOLCHAIN``. Specifically:

    $ cmake -G Ninja \
        -DSWIFT_EXEC=$SWIFT_BUILD_DIR/bin/swiftc \
        -DSWIFT_LIBRARY_PATH=$SWIFT_BUILD_DIR/lib/macosx \
        -DCLANG_EXEC=$LLVM_BUILD_DIR/bin/clang \
        ./swift/benchmark

The complete list of possible options are below:

* `-DSWIFT_EXEC`
    * An absolute path to the Swift driver (``swiftc``) to use to compile the
      benchmarks (default: Xcode's ``swiftc``)
* `-DSWIFT_LIBRARY_PATH`
    * An absolute path to the Swift standard library to use during compilation
      (default: `swiftc_directory`/../lib/swift)
* `-DONLY_PLATFORMS`
    * A list of platforms to build the benchmarks for
      (default: "macosx;iphoneos;appletvos;watchos")
* `-DSWIFT_OPTIMIZATION_LEVELS`
    * A list of Swift optimization levels to build against
      (default: "O;Onone;Osize")
* `-DSWIFT_DARWIN_XCRUN_TOOLCHAIN`
    * The specific toolchain to use when looking up a clang compiler to use when
      linking.
* `-DSWIFT_BENCHMARK_EMIT_SIB`
    * A boolean value indicating whether .sib files should be generated
      alongside .o files (default: FALSE)

The following build targets are available. They are not part of the all target,
so one will need to pass them into ninja oneself.

1. `swift-benchmark-macosx-x86_64`
2. `swift-benchmark-iphoneos-arm64`
3. `swift-benchmark-iphoneos-armv7`
4. `swift-benchmark-appletvos-arm64`
5. `swift-benchmark-watchos-armv7k`

Benchmark driver binaries are placed in `build/bin` and the required Swift
standard library dylibs are placed in `build/lib`. The drivers dynamically link
Swift standard library dylibs from a path relative to their location
(../lib/swift) so the standard library should be distributed alongside them.

## Running Swift Benchmarks

### Using ``build-script``

To run Swift benchmarks, pass the `--benchmark` flag to `build-script`. The
current benchmark results will be compared to the previous run's results if
available. Results for each benchmark run are logged for future comparison.

For branch based development, take a baseline benchmark on the Swift `master`
branch, switch to a development branch containing potentially performance
impacting changes, and run the benchmarks again. Upon benchmark completion, the
benchmark results for the development branch will be compared to the most
recent benchmark results for `master`.

### Using standalone + SwiftPM

When building with SwiftPM, we only build a single binary of the low level
driver (called `SwiftBench`) rather than compile one low level driver for each
individual optimization setting. So it is important to make sure that if one is
building in Xcode that one is building in release mode.

#### swift build

One can build with `swift build` as follows:

```
$ cd ./swift/benchmark
$ swift build -c release
```

This will build `SwiftBench` in release subdirectory `./swift/benchmark/.build`.

#### Xcode

One can generate the Xcode project using SwiftPM as follows:

```
$ cd ./swift/benchmark
$ swift package generate-xcodeproj
generated: ./swiftbench.xcodeproj
$ open -a Xcode ./swift/benchmark/swiftbench.xcodeproj
```

To work with `SwiftBench` from the command line after building in Xcode, notice
that, `SwiftBench` is a build product in Xcode's UI. Thus to find the path of
`SwiftBench`, drag and drop the product from the Xcode window into
Terminal/app. Then one should be able to invoke `SwiftBench` with the relevant
options without problem. E.g.:

```
$ /Users/gottesmm/Library/Developer/Xcode/DerivedData/swiftbench-xxxxxxxxxx/Build/Products/Release/SwiftBench --num-samples 3
```

#### Getting a new enough SwiftPM

To develop benchmarks without compiling a local copy of tip of trunk
SwiftPM/Swift, one can instead download a swift.org [developer
snapshot](https://swift.org/download/#snapshots). This will provide a toolchain
that can be used to generate the xcodeproj and build without building everything
oneself.

### Using standalone + cmake

Run:

```./bin/Benchmark_Driver run -i 3```

### Low Level Driver Usage

`./Benchmark_{O,Onone,Osize} [option] [test_names]`

If test names is empty, then it is assumed that all tests should be run. If the
number of samples is not specified then it is assumed that a sample of 1 is
wanted.

Options:

* ``--num-samples``
    * Control the number of samples to take for each test.
* ``--list``
    * Print a list of available tests
* ``--help``
    * List available options.
* ``--tags=``
    * A comma deliminated list of benchmark tags. Only run benchmarks that are
      related to those tags.
* ``--skip-tags=``
    * Do not skip benchmarks that match the one of the tags in this comma
      deliminated list of tags. To run all
      benchmarks including benchmarks tagged as ``BenchmarkCategory.unstable``,
      pass in the empty string, e.g.: ``--skip-tags=``

Examples:

```
    $ # Run all non-unstable benchmarks for 3 samples each and compute summary statistics.
    $ ./Benchmark_O --num-samples=3
    $ # Print all available benchmarks to stdout.
    $ ./Benchmark_Onone --list
    $ # Run just the benchmark Ackermann
    $ ./Benchmark_Osize Ackermann
    $ # Run all benchmarks including benchmarks tagged as .unstable and .skip.
    $ ./Benchmark_O --skip-tags=
    $ # Run only benchmarks tagged with .Array.
    $ ./Benchmark_O --tags=Array
```

### High Level Driver Usage

```
usage: Benchmark_Driver run [-h] [-f PATTERN] [-t TESTS] [-o OPT]
                            [-i ITERATIONS] [--output-dir OUTPUT_DIR]
                            [--swift-repo SWIFT_REPO]
                            [BENCHMARK [BENCHMARK ...]]

positional arguments:
  BENCHMARK             benchmark to run (default: all)

optional arguments:
  -h, --help            show this help message and exit
  -f PATTERN, --filter PATTERN
                        run all tests whose name match regular expression
                        PATTERN, multiple filters are supported
  -t TESTS, --tests TESTS
                        directory containing Benchmark_O{,none,size} (default:
                        DRIVER_DIR)
  -o OPT, --optimization OPT
                        optimization level to use: {O,Onone,Osize}, (default:
                        O)
  -i ITERATIONS, --iterations ITERATIONS
                        number of times to run each test (default: 1)
  --output-dir OUTPUT_DIR
                        log results to directory (default: no logging)
  --swift-repo SWIFT_REPO
                        absolute path to Swift source repo for branch
                        comparison
```

**NOTE:** The high level driver currently calls the low level driver concept of
"samples" the term "iterations". It should be renamed! Patches welcome!

## Swift Benchmark Developer Guide

### Adding New Benchmarks

To add a new single source test:

1.  Add a new Swift file (`YourTestNameHere.swift`), built according to the
    template below, to the `single-source` directory.
2.  Add the filename of the new Swift file to the list SWIFT_BENCH_MODULES next
    to the rest of the single-source benchmarks in CMakeLists.txt.
3.  Edit `./swift/benchmark/utils/main.swift` such that the test module is
    imported, e.g.: ``import YourTestNameHere`` and the benchmark's benchmark
    info struct or array of benchmark info structs are registered, e.g.:
    ``registerBenchmark(YourTestNameHereBenchmarkInfo)``.

To add a new multiple file test:

1.  Add a new directory and files under the `multi-source` directory as
    specified below:

        +-- multi-source
        |   +-- YourTestName
        |   |   +-- TestFile1.swift
        |   |   +-- TestFile2.swift
        |   |   +-- TestFile3.swift

    At least one file must define a public `YourTestName` variable, initialized to an
    instance of BenchmarkInfo (specified in the template below).

2.  In `CMakeLists.txt` add the new directory name to
    `SWIFT_MULTISOURCE_SWIFT3_BENCHES`, and set `YourTestName_sources` to the
    list of source file paths.

3.  Edit `main.swift`. Import and register your new Swift module as with the
    single-source files.

**NOTE:** we do not need to update the SwiftPM build since our Package.swift will
dynamically create targets based off the file's location. So nothing is needed
as long as we keep our source layout the same.

### The number of iterations per sample

A commonly confused concept when using the low level ``Benchmark_Driver`` is the
distinction in between iteration and sample. All benchmarks in the test suite
take a integral value of ``N`` that is used to scale the benchmark to ensure
that when we run an individual test sample, the test runs are significantly
longer than system jitter. Due to this, we can distinguish in between system
jitter and normal cpu performance.

This is done by the low level benchmark driver. It will measure the time taken for
``N = 1`` and automatically calculate the necessary number of iterations ``N`` to
run each benchmark in approximately one second, so the test should ideally run
in a few milliseconds for ``N = 1``. If the test contains any setup code before
the loop, ensure the time spent on setup is insignificant compared to the time
spent inside the loop (for ``N = 1``) -- otherwise the automatic calculation of
``N`` might be significantly off and any performance gains/regressions will be
masked by the fixed setup time.  If needed you can multiply ``N`` by a fixed amount
(e.g. ``1...100*N``) to achieve this.

### BenchmarkInfo

The benchmark info struct that one uses to register a benchmark has the
following interface:

```{.sourceCode .swift}
public struct BenchmarkInfo {
  /// The name of the benchmark that should be displayed by the harness.
  public var name: String

  /// A function that invokes the specific benchmark routine.
  public var runFunction: ((Int) -> ())?

  /// A set of category tags that describe this benchmark. This is used by the
  /// harness to allow for easy slicing of the set of benchmarks along tag
  /// boundaries, e.x.: run all string benchmarks or ref count benchmarks, etc.
  public var tags: [BenchmarkCategory]

  /// The platforms that this benchmark supports. This is an OptionSet.
  private var unsupportedPlatforms: BenchmarkPlatformSet

  /// An optional function that if non-null is run before benchmark samples
  /// are timed.
  public var setUpFunction : (() -> ())?

  /// An optional function that if non-null is run immediately after a sample is
  /// taken.
  public var tearDownFunction: (() -> ())?

  public init(name: String, runFunction: @escaping (Int) -> (), tags: [BenchmarkCategory],
              setUpFunction: (() -> ())? = nil,
              tearDownFunction: (() -> ())? = nil,
              unsupportedPlatforms: BenchmarkPlatformSet = [])
}
```

The high level information here is:

1. The benchmark harness will execute the routine referenced by ``runFunction``.
2. ``setUpFunction``, ``tearDownFunction`` will not be run during the timed
   portion of the benchmark by the benchmark harness. Please use it to eliminate
   unwanted setup overhead and cache any such data structures in a optional
   global that can be accessed unsafely without overhead.
3. The current set of ``tags`` are defined by the `BenchmarkCategory` enum in
   `./swift/benchmark/utils/TestsUtils.swift` .


### Regenerating gybed tests using the generate_harness script.

`scripts/generate_harness/generate_harness.py` runs `gyb` to automate generation
of some benchmarks.

**FIXME:** `gyb` should be invoked automatically during the build so that
manually invoking `generate_harness.py` is not required.

### Performance Test Template

Single benchmark:

```{.sourceCode .swift}
// YourTestName benchmark
//
// rdar://problem/00000000
import TestsUtils

public let YourTestName = BenchmarkInfo(
  name: "YourTestName",
  runFunction: run_YourTestName,
  tags: [.regression])

@inline(never)
public func run_YourTestName(N: Int) {
    # Declare variables
    ...

    for i in 1...N {
        # Perform work
        ...

        # Verify work was done; break otherwise
        ...
    }

    # Assert with CheckResults that work was done
    ...
}
```

A/B Test:

``` {.sourceCode .swift}
// MyABTest benchmark
//
// rdar://problem/00000000

import TestsUtils

public let MyABTest = [
    BenchmarkInfo(
        name: "YourTestName_WithA",
        runFunction: run_YourTestNameA,
        tags: [.regression]),
    BenchmarkInfo(
        name: "YourTestName_WithB",
        runFunction: run_YourTestNameB,
        tags: [.regression]),
]

@inline(never)
public func run_YourTestNameA(N: Int) { ... }

@inline(never)
public func run_YourTestNameB(N: Int) { ... }
```
