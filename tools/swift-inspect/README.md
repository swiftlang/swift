# swift-inspect

swift-inspect is a debugging tool which allows you to inspect a live Swift process to gain insight into the runtime interactions of the application.

swift-inspect uses the reflection APIs to introspect the live process.  It relies on the swift remote mirror library to remotely reconstruct data types.

### Building

All platforms require [swift-package-manager](https://github.com/apple/swift-package-manager).

##### macOS

On macOS, use provided `build_script_helper.py`.

##### Windows

In order to build on Windows, some additional parameters must be passed to the build tool to locate the necessary libraries.

~~~
swift build -Xcc -I%SDKROOT%\usr\include\swift\SwiftRemoteMirror -Xlinker %SDKROOT%\usr\lib\swift\windows\x86_64\swiftRemoteMirror.lib
~~~

##### 64bit Linux

The process is a little bit more complicated on Linux. Some features available on macOS and Windows are not present on Linux and additional libraries are needed.

In order to function correctly, `swift-inspect` needs the ability to enumerate memory allocated on the heap. This is not done simply under `Glibc` which is the C library by Swift. The ability to enumerate heap is made available via 3rd party tool called `memtool`. This tool was develop specifically for `swift-inspect`.

**However, `memtool` is not maintained by the Swift maintainers and should be used with caution and at your own risk.**

Before downloading, compiling and executing `memtool`, be sure you are familiar with [important considerations](https://github.com/mikolasstuchlik/memtool/#important-considerations).

**Installation**

1. Check-out and build
```bash
git checkout https://github.com/mikolasstuchlik/memtool.git
cd memtool
swift build
```

2. Build `swift-inspect` 

```bash
swift build --package-path . \
            --build-path .build \
            --configuration debug \
            -Xswiftc -I -Xswiftc $SWIFT_REPO_ROOT/swift/include/swift \
            -Xswiftc -I -Xswiftc $MEMTOOL_REPO_ROOT/.build/x86_64-unknown-linux-gnu/debug \
            -Xswiftc -L -Xswiftc $SWIFT_TOOLCHAIN_PATH/usr/lib/swift/linux \
            -Xswiftc -L -Xswiftc $MEMTOOL_REPO_ROOT/.build/x86_64-unknown-linux-gnu/debug \
            -Xswiftc -lswiftRemoteMirror \
            -Xswiftc -lMemtoolCore \
            -Xlinker -rpath -Xlinker $SWIFT_TOOLCHAIN_PATH/usr/lib/swift/linux \
            -Xlinker -rpath -Xlinker $MEMTOOL_REPO_ROOT/.build/x86_64-unknown-linux-gnu/debug
```

This build command requires includes (lines with `-I`) from Memtool, but also from the Swift repository, since those includes are not shipped with toolcahin. It also needs to link `libswiftRemoteMirror.so` from the Swift toolchain and `libMemtoolCore.so` from the `memtool` build folder.

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

### Example usage

##### 64bit Linux

Consider this simple demonstration program

```swift
// simple.swift
final class MyClass {
    var aSelf: MyClass?

    var a: UInt = 0xabababababababab
    var b: UInt = 0xcdcdcdcdcdcdcdcd
    var c: UInt = 0xefefefefefefefef

    init() {
        aSelf = self
    }
}

var myArray: [UInt] = Array()
myArray.reserveCapacity(64)
for _ in 0..<64 {
    myArray.append(0xabcdabcdabcdabcd)
}

_ = MyClass()
_ = MyClass()

_ = readLine()
```
In order to trace it, we would need to compile it, run it and find the PID:
```bash
$ swiftc simple.swift
$ ./simple
$ ps aux | grep simple
you    14808  0.0  0.0  13572  4040 pts/0    T    17:49   0:00 ./simple
you    37034  0.0  0.0  20264  2360 pts/3    S+   21:17   0:00 grep --color=auto simple
```

The PID is `14808`.
Now enter the directory containing the `swift-inspect` package and execute (`sudo` may be required)

```
$ .build/debug/swift-inspect dump-arrays 14808
Address	Size	Count	Is Class
0x563da5bbb770	552	64	false
```

In order to run `dump-raw-metadata`, `dump-generic-metadata` and other modes, you will need to set the `SWIFT_DEBUG_ENABLE_METADATA_ALLOCATION_ITERATION` environment variable. Swift Runtime may optionally enable some features based on environment variables, [refer to the compiler source code for more information](https://github.com/apple/swift/blob/main/stdlib/public/runtime/EnvironmentVariables.def).

```bash
$ export SWIFT_DEBUG_ENABLE_METADATA_ALLOCATION_ITERATION=1 && ./simple
$ ps aux | grep simple
you    14809  0.0  0.0  13572  4040 pts/0    T    17:49   0:00 ./simple
you    37035  0.0  0.0  20264  2360 pts/3    S+   21:17   0:00 grep --color=auto simple
```

Following execution contains demonstration of the modes mentioned above (`sudo may be required`):
```bash
$ .build/debug/swift-inspect dump-raw-metadata 14809
Metadata allocation at: 0x7f39f704dc88 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704dcb0 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704dcd8 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704dd00 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704dd28 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704dd50 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704dd78 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704dda0 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704ddc8 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704ddf0 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704de18 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704de40 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704de68 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704de90 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704deb8 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704dee0 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704df08 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704df30 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704df58 size: 32 tag: 14 (GenericMetadataCache)
Metadata allocation at: 0x7f39f704df80 size: 32 tag: 14 (GenericMetadataCache)
$ .build/debug/swift-inspect dump-generic-metadata 14809
Address	Allocation	Size	Offset	isArrayOfClass	Name
0x7f39f6f5a0f0	???	??	???	false	Swift.AnyObject
0x7f39f6f53960	???	??	???	false	Swift._DictionaryCodingKey
0x7f39f6f53bd0	???	??	???	false	Swift.Range<Swift.Int>
0x7f39f6f50be0	???	??	???	false	Swift.Int8
0x7f39f6f4f168	???	??	???	false	Swift.String.Index
0x7f39f6f56790	???	??	???	false	Swift.(_V in $7f39f6f7f4e0)
0x7f39f6f50e60	???	??	???	false	Swift.Int
0x7f39f6f4db38	???	??	???	false	Swift._HashTable.Bucket
0x7f39f6f4cb28	???	??	???	false	Swift.Character
0x7f39f6f50c30	???	??	???	false	Swift.UInt16
0x7f39f6f50cd0	???	??	???	false	Swift.UInt32
0x7f39f6f4dee8	???	??	???	false	Swift.Unicode.Scalar
0x7f39f6f50e10	???	??	???	false	Swift.UInt
0x7f39f6f4ecf8	???	??	???	false	Swift.StaticString
0x7f39f6f50b90	???	??	???	false	Swift.UInt8
0x7f39f6f4eee0	???	??	???	false	Swift.String
0x7f39f6f50dc0	???	??	???	false	Swift.Int64
0x7f39f6f552d0	???	??	???	false	Swift.Dictionary<Swift.String, Swift.AnyObject>
0x7f39f6f4d968	???	??	???	false	Swift.AnyHashable
0x7f39f6f5a0d8	???	??	???	false	Any
```
