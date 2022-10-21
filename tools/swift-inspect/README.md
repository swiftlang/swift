# disclaimer
This is **work in progress** version of `swift-inpect` which depends on [mikolasstuchlik/memtool](https://github.com/mikolasstuchlik/memtool). I hesistate to (and probably never will) include this package in the SPM manifest of `swift-inspect`.
Check out this package and build it - then link the products manually via the helper script.

```bash
# Example
python3 build_script_helper.py --package-path . \
                               --build-path .build \
                               --toolchain /usr/local/bin/swift/ \
                               --configuration debug \
                               --memtool-product /home/mikolas/Developer/ptrace/memtool/.build/x86_64-unknown-linux-gnu/debug \
                               --swift-repo /home/mikolas/Developer/swift5.7/swift
```

The helper script provided with this WIP version works on linux only and requires `--memtool-product` path to the directory containing `.swiftmodule` and `.so` of `MemtoolCore` and other dependencies. The `--swift-repo` path should lead to a checkout of the `apple/swift` repository.

In the default configuration, log of each method execution is printed on stdout. If you don't wish to see it, remove the `-Xswiftc -DDIAGNOSTIC`.

# swift-inspect

swift-inspect is a debugging tool which allows you to inspect a live Swift process to gain insight into the runtime interactions of the application.

swift-inspect uses the reflection APIs to introspect the live process.  It relies on the swift remote mirror library to remotely reconstruct data types.

### Building

swift-inspect can be built using [swift-package-manager](https://github.com/apple/swift-package-manager).

##### Windows

In order to build on Windows, some additional parameters must be passed to the build tool to locate the necessary libraries.

~~~
swift build -Xcc -I%SDKROOT%\usr\include\swift\SwiftRemoteMirror -Xlinker %SDKROOT%\usr\lib\swift\windows\x86_64\swiftRemoteMirror.lib
~~~

### Using

The following inspection operations are available currently.

##### All Platforms

dump-arrays &lt;name-or-pid&gt;
: Print information about array objects in the target

dump-cache-nodes &lt;name-or-pid&gt;
: Print the metadata cache nodes.

dump-conformance-cache &lt;name-or-pid&gt;
: Print the content of the protocol conformance cache.

dump-generic-metadata &lt;name-or-pid&gt; [--backtrace] [--backtrace-long]
: Print generic metadata allocations.

dump-raw-metadata &lt;name-or-pid&gt; [--backtrace] [--backtrace-long]
: Print metadata allocations.

##### Darwin Only

dump-concurrency &lt;name-or-pid&gt;
: Print information about tasks, actors, and threads under Concurrency.
