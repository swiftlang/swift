# Building Swift SDK for Android on Windows

Visual Studio 2019 or newer is needed to build the Swift SDK for Android on
Windows.

## 1. Install Dependencies
- Install the latest version of [Visual Studio](https://www.visualstudio.com/downloads/)
- Make sure to include the android NDK in your installation.

## 1. Clone the repositories
1. Configure git to work with Unix file endings
1. Clone `apple/swift-llvm` into a directory named `llvm`
1. Clone `apple/swift-corelibs-libdispatch` into a directory named `swift-corelibs-libdispatch`
1. Clone `apple/swift-corelibs-foundation` into a directory named `swift-corelibs-foundation`G
1. Clone `apple/swift-corelibs-xctest` into a directory named `swift-corelibs-xctest`
1. Clone `compnerd/swift-windows` into a directory named `swift-windows`

- Currently, other repositories in the Swift project have not been tested and
  may not be supported.

This guide assumes that your sources live at the root of `S:`.  If your sources
live elsewhere, you can create a substitution for this:

```cmd
subst S: <path to sources>
```

```cmd
S:
git clone https://github.com/apple/swift-llvm llvm
git clone https://github.com/apple/swift-corelibs-libdispatch swift-corelibs-libdispatch
git clone https://github.com/apple/swift-corelibs-foundation swift-corelibs-foundation
git clone https://github.com/apple/swift-corelibs-xctest swift-corelibs-xctest
git clone https://github.com/compnerd/swift-windows swift-windows
```

## 1. Acquire the lastest toolchain and dependencies

1. Download the toolchain, ICU, libxml2, and curl for android from
   [Azure](https://dev.azure.com/compnerd/windows-swift) into `S:\b\a\Library`.

- You can alternatively use `Download-AndroidArtifacts.ps1` from
  [compnerd/windows-swift](https://www.github.com/compnerd/windows-swift) under
  the utilities directory.  This will implicitly setup the requisite directory
  structure.

## 1. Configure LLVM

```cmd
md S:\b\a\llvm
cd S:\b\a\llvm
cmake -C S:\swift-windows\cmake\caches\android-armv7.cmake                                                      ^
  -G Ninja                                                                                                      ^
  -DCMAKE_BUILD_TYPE=Release                                                                                    ^
  -DCMAKE_TOOLCHAIN_FILE=S:\swift-windows\cmake\toolchains\android.toolchain.cmake                              ^
  -DANDROID_ALTERNATE_TOOLCHAIN=S:/b/a/Library/Developer/Toolchains/unknown-Asserts-development.xctoolchain/usr ^
  -DLLVM_HOST_TRIPLE=armv7-unknown-linux-androideabi                                                            ^
  S:/llvm
```

## 1. Build and install the standard library

- We must build and install the standard library to build the remainder of the
  SDK

```cmd
md S:\b\a\stdlib
cd S:\b\a\stdlib
cmake -C S:\windows-swift\cmake\caches\android-armv7.cmake                                                            ^
  -C S:\windows-swift\cmake\caches\swift-stdlib-android-armv7.cmake                                                   ^
  -G Ninja                                                                                                            ^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo                                                                                   ^
  -DCMAKE_INSTALL_PREFIX=S:/b/a/Library/Developer/Platforms/android.platform/Developer/SDKs/android.sdk/usr           ^
  -DCMAKE_TOOLCHAIN_FILE=S:\windows-swift\cmake\toolchains\android.toolchain.cmake                                    ^
  -DANDROID_ALTERNATE_TOOLCHAIN=S:/b/a/Library/Developer/Toolchains/unknown-Asserts-development.xctoolchain/usr       ^
  -DLLVM_DIR=S:/b/a/llvm/lib/cmake/llvm                                                                               ^
  -DSWIFT_NATIVE_SWIFT_TOOLS_PATH=S:/b/a/Library/Developer/Toolchains/unknown-Asserts-development.xctoolchain/usr/bin ^
  -DSWIFT_ANDROID_armv7_ICU_UC_INCLUDE=S:/b/a/Library/icu-64/usr/include/unicode                                      ^
  -DSWIFT_ANDROID_armv7_ICU_UC=S:/b/a/Library/icu-64/usr/lib/libicuuc64.so                                            ^
  -DSWIFT_ANDROID_armv7_ICU_I18N_INCLUDE=S:/b/a/Library/icu-64/usr/include                                            ^
  -DSWIFT_ANDROID_armv7_ICU_I18N=S:/b/a/Library/icu-64/usr/lib/libicuin64.so                                          ^
  S:/swift
ninja
ninja install
```

## 1. Build libdispatch

- We *cannot* install libdispatch until after all builds are complete as that
  will cause the Dispatch module to be imported twice and fail to build.

```cmd
md S:\b\a\libdispatch
cd S:\b\a\libdispatch
cmake -C S:\windows-swift\cmake\caches\android-armv7.cmake                                                              ^
  -DSWIFT_ANDROID_SDK=S:/b/a/Library/Developer/Platforms/android.platform/Developer/SDKs/android.sdk                    ^
  -C S:\windows-swift\cmake\caches\android-armv7-swift-flags.cmake                                                      ^
  -G Ninja                                                                                                              ^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo                                                                                     ^
  -DCMAKE_INSTALL_PREFIX=S:/b/a/Library/Developer/Platforms/android.platform/Developer/SDKs/android.sdk/usr             ^
  -DCMAKE_SWIFT_COMPILER=S:/b/a/Library/Developer/Toolchains/unknown-Asserts-development.xctoolchain/usr/bin/swiftc.exe ^
  -DCMAKE_TOOLCHAIN_FILE=S:\windows-swift\cmake\toolchains\android.toolchain.cmake                                      ^
  -DANDROID_ALTERNATE_TOOLCHAIN=S:/b/a/Library/Developer/Toolchains/unknown-Asserts-development.xctoolchain/usr         ^
  -DENABLE_SWIFT=YES                                                                                                    ^
  -DENABLE_TESTING=NO                                                                                                   ^
  S:/swift-corelibs-libdispatch
ninja
```

## 1. Build foundation

```cmd
md S:\b\a\foundation
cd S:\b\a\foundation
cmake -C S:\windows-swift\cmake\caches\android-armv7.cmake                                                              ^
  -DSWIFT_ANDROID_SDK=S:/b/a/Library/Developer/Platforms/android.platform/Developer/SDKs/android.sdk                    ^
  -C S:\windows-swift\cmake\caches\android-armv7-swift-flags.cmake                                                      ^
  -G Ninja                                                                                                              ^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo                                                                                     ^
  -DCMAKE_INSTALL_PREFIX=S:/b/a/Library/Developer/Platforms/android.platform/Developer/SDKs/android.sdk/usr             ^
  -DCMAKE_SWIFT_COMPILER=S:/b/a/Library/Developer/Toolchains/unknown-Asserts-development.xctoolchain/usr/bin/swiftc.exe ^
  -DCMAKE_TOOLCHAIN_FILE=S:\windows-swift\cmake\toolchains\android.toolchain.cmake                                      ^
  -DANDROID_ALTERNATE_TOOLCHAIN=S:/b/a/Library/Developer/Toolchains/unknown-Asserts-development.xctoolchain/usr         ^
  -DCURL_LIBRARY=S:/b/a/Library/libcurl-development/usr/lib/libcurl.a                                                   ^
  -DCURL_INCLUDE_DIR=S:/b/a/Library/libcurl-development/usr/include                                                     ^
  -DICU_INCLUDE_DIR=S:/b/a/Library/icu-64/usr/include                                                                   ^
  -DICU_UC_LIBRARY=S:/b/a/Library/icu-64/usr/lib/libicuuc64.so                                                          ^
  -DICU_UC_LIBRARY_RELEASE=S:/b/a/Library/icu-64/usr/lib/libicuuc64.so                                                  ^
  -DICU_I18N_LIBRARY=S:/b/a/Library/icu-64/usr/lib/libiucin64.so                                                        ^
  -DICU_I18N_LIBRARY_RELEASE=S:/b/a/Library/icu-64/usr/lib/libicuin64.so                                                ^
  -DLIBXML2_LIBRARY=S:/b/a/Library/libxml2-development/usr/lib/libxml2.a                                                ^
  -DLIBXML2_INCLUDE_DIR=S:/b/a/Library/libxml2-development/usr/include/libxml2                                          ^
  -DFOUNDATION_PATH_TO_LIBDISPATCH_SOURCE=S:/swift-corelibs-libdispatch                                                 ^
  -DFOUNDATION_PATH_TO_LIBDISPATCH_BUILD=S:/b/a/libdispatch                                                             ^
  S:/swift-corelibs-foundation
ninja
```

## 1. Build XCTest

```cmd
md S:\b\a\xctest
cd S:\b\a\xctest
cmake -C S:\swift-windows\cmake\caches\android-armv7.cmake                                                              ^
  -C S:\swift-windows\cmake\caches\android-armv7-swift-flags.cmake                                                      ^
  -G Ninja                                                                                                              ^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo                                                                                     ^
  -DCMAKE_INSTALL_PREFIX=S:/b/a/Library/Developer/Platforms/android.platform/Developer/SDKs/android.sdk/usr             ^
  -DCMAKE_SWIFT_COMPILER=S:/b/a/Library/Developer/Toolchains/unknown-Asserts-development.xctoolchain/usr/bin/swiftc.exe ^
  -DCMAKE_TOOLCHAIN_FILE=S:\swift-windows\cmake\toolchains\android.toolchain.cmake                                      ^
  -DANDROID_ALTERNATE_TOOLCHAIN=S:/b/a/Library/Developer/Toolchains/unknown-Asserts-development.xctoolchain/usr         ^
  -DSWIFT_ANDROID_SDK=S:/b/a/Library/Developer/Platforms/andrfoid.platform/Developer/SDKs/android.sdk                   ^
  -DXCTEST_PATH_TO_FOUNDATION_BUILD=S:/b/a/foundation                                                                   ^
  -DXCTEST_PATH_TO_LIBDISPATCH_SOURCE=S:/swift-corelibs-libdispatch                                                     ^
  -DXCTEST_PATH_TO_LIBDISPATCH_BUILD=S:/b/a/libdispatch                                                                 ^
  -DENABLE_TESTING=NO                                                                                                   ^
  S:/swift-corelibs-foundation
ninja
```

## 1. Install libdispatch

```cmd
cd S:\b\a\libdispatch
ninja install
```

## 1. Install Foundation

```cmd
cd S:\b\a\foundation
ninja install
```

## 1. Install XCTest

```cmd
cd S:\b\a\xctest
ninja install
```

