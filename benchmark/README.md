Swift Benchmark Suite
=====================

This directory contains the Swift Benchmark Suite.

Running Swift Benchmarks
------------------------

To run Swift benchmarks, pass the `--benchmark` flag to `build-script`. The
current benchmark results will be compared to the previous run's results if
available. Results for each benchmark run are logged for future comparison.

For branch based development, take a baseline benchmark on the Swift `master`
branch, switch to a development branch containing potentially performance
impacting changes, and run the benchmarks again. Upon benchmark completion, the
benchmark results for the development branch will be compared to the most
recent benchmark results for `master`.

Building with build-script
--------------------------

By default, Swift benchmarks for OS X are compiled during the Swift build
process. To build Swift benchmarks for additional platforms, pass the following
flags:

    $ swift/utils/build-script --ios --watchos --tvos

OS X benchmark driver binaries are placed in `bin` alongside `swiftc`.
Additional platform binaries are placed in the `benchmark/bin` build directory.

Building Independently
----------------------

To build the Swift benchmarks using only an Xcode installation: install an
Xcode version with Swift support, install cmake 2.8.12, and ensure Xcode is
selected with xcode-select.

The following build options are available:

* `-DSWIFT_EXEC`
    * An absolute path to the Swift driver (`swiftc`) to use to compile the
      benchmarks (default: Xcode's `swiftc`)
* `-DSWIFT_LIBRARY_PATH`
    * An absolute path to the Swift standard library to use during compilation
      (default: `swiftc_directory`/../lib/swift)
* `-DONLY_PLATFORMS`
    * A list of platforms to build the benchmarks for
      (default: "macosx;iphoneos;appletvos;watchos")
* `-DSWIFT_OPTIMIZATION_LEVELS`
    * A list of Swift optimization levels to build against
      (default: "O;Onone;Osize")
* `-DSWIFT_BENCHMARK_EMIT_SIB`
    * A boolean value indicating whether .sib files should be generated
      alongside .o files (default: FALSE)

The following build targets are available:

1. `swift-benchmark-macosx-x86_64`
2. `swift-benchmark-iphoneos-arm64`
3. `swift-benchmark-iphoneos-armv7`
4. `swift-benchmark-appletvos-arm64`
5. `swift-benchmark-watchos-armv7k`

Build steps (with example options):

1. `$ cd benchmark`
2. `$ mkdir build`
3. `$ cd build`
4. `$ cmake ..`
5. `$ make -j8 swift-benchmark-macosx-x86_64`

Benchmark driver binaries are placed in `build/bin` and the required Swift
standard library dylibs are placed in `build/lib`. The drivers dynamically link
Swift standard library dylibs from a path relative to their location
(../lib/swift) so the standard library should be distributed alongside them.

Using the Benchmark Driver
--------------------------

### Usage

`./Driver [ test_name [ test_name ] ] [ option [ option ] ]`

* `--num-iters`
    * Control the number of loop iterations in each test sample
* `--num-samples`
    * Control the number of samples to take for each test
* `--list`
    * Print a list of available tests matching specified criteria
* `--tags`
    * Run tests that are labeled with specified [tags](https://github.com/apple/swift/blob/master/benchmark/utils/TestsUtils.swift#L19)
     (comma separated list); multiple tags are interpreted as logical AND, i.e.
     run only test that are labeled with all the supplied tags
* `--skip-tags`
    * Don't run tests that are labeled with any of the specified tags (comma
      separated list); default value: `skip,unstable`; to get complete list of
      tests, specify empty `--skip-tags=`


### Examples

* `$ ./Benchmark_O --num-iters=1 --num-samples=1`
* `$ ./Benchmark_Onone --list`
* `$ ./Benchmark_Osize Ackermann`
* `$ ./Benchmark_O --tags=Dictionary`
* `$ ./Benchmark_O --skip-tags=unstable,skip,validation`

### Note
As a shortcut, you can also refer to benchmarks by their ordinal numbers.
These are printed out together with benchmark names and tags using the
`--list` parameter. For a complete list of all available performance tests run
* `$ ./Benchmark_O --list --skip-tags=`

You can use test numbers instead of test names like this:
* `$ ./Benchmark_O 1 42`
* `$ ./Benchmark_Driver run 1 42`

Test numbers are not stable in the long run, adding and removing tests from the
benchmark suite will reorder them, but they are stable for a given build.

Using the Harness Generator
---------------------------

`scripts/generate_harness/generate_harness.py` runs `gyb` to automate generation
of some benchmarks.

** FIXME ** `gyb` should be invoked automatically during the
   build so that manually invoking `generate_harness.py` is not required.

Adding New Benchmarks
---------------------

The harness generator supports both single and multiple file tests.

To add a new single file test:

1.  Add a new Swift file (`YourTestNameHere.swift`), built according to
    the template below, to the `single-source` directory.
2.  Add the filename of the new Swift file to CMakeLists.txt
3.  Edit `main.swift`. Import and register your new Swift module.

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

3.  Edit `main.swift`. Import and register your new Swift module.

**Note:**

The benchmark harness will execute the routine referenced by
`BenchmarkInfo.runFunction`.

The benchmark driver will measure the time taken for `N = 1` and automatically calculate
the necessary number of iterations `N` to run each benchmark in approximately one second,
so the test should ideally run in a few milliseconds for `N = 1`. If the test contains
any setup code before the loop, ensure the time spent on setup is insignificant compared to
the time spent inside the loop (for `N = 1`) -- otherwise the automatic calculation of `N` might be
significantly off and any performance gains/regressions will be masked by the fixed setup time.
If needed you can multiply N by a fixed amount (e.g. `1...100*N`) to achieve this.


**Performance Test Template**

``` {.sourceCode .swift}
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

    for i in 1...N {
        # Perform work

        # Verify work was done; break otherwise
    }

    # Assert with CheckResults that work was done
}
```

The current set of tags are defined by the `BenchmarkCategory` enum in
`TestsUtils.swift` .

Testing the Benchmark Drivers
-----------------------------
When working on tests, after the initial build
````
swift-source$ ./swift/utils/build-script -R -B
````
you can rebuild just the benchmarks:
````
swift-source$ export SWIFT_BUILD_DIR=`pwd`/build/Ninja-ReleaseAssert/swift-macosx-x86_64
swift-source$ ninja -C ${SWIFT_BUILD_DIR} swift-benchmark-macosx-x86_64
````

When modifying the testing infrastructure, you should verify that your changes
pass all the tests:
````
swift-source$ ./llvm/utils/lit/lit.py -sv ${SWIFT_BUILD_DIR}/test-macosx-x86_64/benchmark
````
