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
      (default: "O;Onone;Ounchecked")
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
    * Print a list of available tests

### Examples

1. `$ ./Benchmark_O --num-iters=1 --num-samples=1`
2. `$ ./Benchmark_Onone --list`
3. `$ ./Benchmark_Ounchecked Ackermann`

Using the Harness Generator
---------------------------

`scripts/generate_harness/generate_harness.py` generates and replaces
`CMakeLists.txt` and `utils/main.swift` from single and multiple file tests
contained in the directories `single-source` and `multi-source`. It gathers
information about the tests and then generates the files from templates using
jinja2. The motivation for creating this script was to eliminate the need to
manually add at least three lines to harness files (one to `CMakeLists.txt` and
two to `utils/main.swift`) for every new benchmark added.

**Warning:**

Since `CMakeLists.txt` and `utils/main.swift` are now generated from templates,
they should not be directly modified. Work may be lost if the harness is
executed after making changes to derived files. Instead, modifications should
be made to the template files stored in the `scripts/generate_harness`
directory.

### Generating harness files

Start by installing jinja2 if it isn't already installed:

    $ sudo easy_install -U jinja2

To generate `CMakeLists.txt` and `utils/main.swift` from test sources, run the
command:

    $ scripts/generate_harness/generate_harness.py

**Note:**

Ensure `generate_harness.py` remains in `scripts/generate_harness` as it
modifies files relative to its location instead of the current working
directory.

### Modifying CMakeLists.txt or utils/main.swift

To make changes to `CMakeLists.txt` or `utils/main.swift`, modify the template
files `CMakeLists.txt_template` and `main.swift_template` stored in the
`scripts/generate_harness` directory. These are jinja2 templates, rendered by
jinja2 calls in `generate_harness.py`, so ensure static changes don't interfere
with the template portions. Test changes by regenerating the harness
(*Generating harness files*) and rebuilding the repository with `build-script`.

Adding New Benchmarks
---------------------

The harness generator supports both single and multiple file tests.

To add a new single file test:

1.  Add a new Swift file (`YourTestNameHere.swift`), built according to
    the template below, to the `single-source` directory.
2.  Regenerate harness files by following the directions in
    *Generating harness files* before committing changes.

To add a new multiple file test:

1.  Add a new directory and files under the `multi-source` directory as
    specified below:

        +-- multi-source
        |   +-- YourTestName
        |   |   +-- TestFile1.swift
        |   |   +-- TestFile2.swift
        |   |   +-- TestFile3.swift

    At least one run function (specified in the template below) must
    exist in the files.

2.  Regenerate harness files by following the directions in
    *Generating harness files* before committing changes.

**Note:**

The generator script looks for functions prefixed with `run_` in order to
populate `utils/main.swift`.

The benchmark driver will measure the time taken for `N = 1` and automatically calculate
the necessary number of iterations `N` to run each benchmark in approximately one second,
so the test should ideally run in a few milliseconds for `N = 1`. If the test contains
any setup code before the loop, ensure the time spent on setup is insignificant compared to
the time spent inside the loop (for `N = 1`) -- otherwise the automatic calculation of `N` might be
significantly off and any performance gains/regressions will be masked by the fixed setup time.
If needed you can multiply N by a fixed amount (e.g. `1...100*N`) to achieve this.


**Performance Test Template**

``` {.sourceCode .swift}
// YourTestNameHere benchmark
//
// rdar://problem/00000000
import Foundation
import TestsUtils

@inline(never)
public func run_YourTestNameHere(N: Int) {
    # Declare variables

    for i in 1...N {
        # Perform work

        # Verify work was done; break otherwise
    }

    # Assert with CheckResults that work was done
}
```

