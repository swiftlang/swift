# swift-inspect

swift-inspect is a debugging tool which allows you to inspect a live Swift process to gain insight into the runtime interactions of the application.

swift-inspect uses the reflection APIs to introspect the live process.  It relies on the swift remote mirror library to remotely reconstruct data types.

### Building

swift-inspect can be built using [swift-package-manager](https://github.com/swiftlang/swift-package-manager).

#### Windows

In order to build on Windows, some additional parameters must be passed to the build tool to locate the necessary libraries.

~~~
swift build -Xcc -I%SDKROOT%\usr\include\swift\SwiftRemoteMirror -Xlinker %SDKROOT%\usr\lib\swift\windows\x86_64\swiftRemoteMirror.lib
~~~

#### Linux

In order to build on Linux, some additional parameters must be passed to the build tool to locate the necessary includes and libraries.

~~~
swift build -Xswiftc -I$(git rev-parse --show-toplevel)/include/swift/SwiftRemoteMirror -Xlinker -lswiftRemoteMirror
~~~

#### CMake

In order to build on Windows with CMake, some additional parameters must be passed to the build tool to locate the necessary Swift modules.

~~~
cmake -B out -G Ninja -S . -D ArgumentParser_DIR=... -D CMAKE_Swift_FLAGS="-Xcc -I%SDKROOT%\usr\include\swift\SwiftRemoteMirror"
~~~

#### Android

To cross-compile swift-inspect for Android, some additional parameters must be passed to the build tool to locate the toolchain and necessary libraries.

~~~
set ANDROID_ARCH=aarch64
set ANDROID_API_LEVEL=29
set ANDROID_NDK_ROOT=C:\Android\android-sdk\ndk\26.3.11579264
set SWIFT_ANDROID_SDK_ROOT=C:\Users\Andrew\AppData\Local\Programs\Swift\Platforms\0.0.0\Android.platform\Developer\SDKs\Android.sdk
swift build --triple %ANDROID_ARCH%-unknown-linux-android%ANDROID_API_LEVEL% `
    --sdk %ANDROID_NDK_ROOT%\toolchains\llvm\prebuilt\windows-x86_64\sysroot `
    -Xswiftc -sdk -Xswiftc %SWIFT_ANDROID_SDK_ROOT% `
    -Xswiftc -sysroot -Xswiftc %ANDROID_NDK_ROOT%\toolchains\llvm\prebuilt\windows-x86_64\sysroot `
    -Xswiftc -I -Xswiftc %SWIFT_ANDROID_SDK_ROOT%\usr\include `
    -Xlinker -L%ANDROID_NDK_ROOT%\toolchains\llvm\prebuilt\windows-x86_64\lib\clang\17.0.2\lib\linux\%ANDROID_ARCH% `
    -Xcc -I%SWIFT_ANDROID_SDK_ROOT%\usr\include\swift\SwiftRemoteMirror `
    -Xlinker %SWIFT_ANDROID_SDK_ROOT%\usr\lib\swift\android\%ANDROID_ARCH%\libswiftRemoteMirror.so
~~~

### Using

The following inspection operations are available currently.

##### All Platforms

dump-cache-nodes &lt;name-or-pid&gt;
: Print the metadata cache nodes.

dump-conformance-cache &lt;name-or-pid&gt;
: Print the content of the protocol conformance cache.

dump-generic-metadata &lt;name-or-pid&gt; [--backtrace] [--backtrace-long]
: Print generic metadata allocations.

dump-raw-metadata &lt;name-or-pid&gt; [--backtrace] [--backtrace-long]
: Print metadata allocations.

#### Darwin and Windows Only

dump-arrays &lt;name-or-pid&gt;
: Print information about array objects in the target

##### Darwin Only

dump-concurrency &lt;name-or-pid&gt;
: Print information about tasks, actors, and threads under Concurrency.
