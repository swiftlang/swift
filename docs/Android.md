# Getting Started with Swift on Android

The Swift stdlib can be compiled for Android armv7 targets, which makes it
possible to execute Swift code on a mobile device running Android. This guide
explains:

1. How to run a simple "Hello, world" program on your Android device.
2. How to run the Swift test suite, targeting Android, and on an Android device.

If you encounter any problems following the instructions below, please file a
bug using https://bugs.swift.org/.

## FAQ

Let's answer a few frequently asked questions right off the bat:

### Does this mean I can write Android applications in Swift?

No. Although the Swift compiler is capable of compiling Swift code that runs
on an Android device, it takes a lot more than just the Swift stdlib to write
an app. You'd need some sort of framework to build a user interface for your
application, which the Swift stdlib does not provide.

Alternatively, one could theoretically call into Java interfaces from Swift,
but unlike as with Objective-C, the Swift compiler does nothing to facilitate
Swift-to-Java bridging.

## Prerequisites

To follow along with this guide, you'll need:

1. A Linux environment capable of building Swift from source, specifically
   Ubuntu 16.04 or Ubuntu 15.10 (Ubuntu 14.04 has not been tested recently).
   The stdlib is currently only buildable for Android from a Linux environment.
   Before attempting to build for Android, please make sure you are able to build
   for Linux by following the instructions in the Swift project README.
2. The latest version of the Android NDK (r14 at the time of this writing),
   available to download here:
   http://developer.android.com/ndk/downloads/index.html.
3. An Android device with remote debugging enabled. We require remote
   debugging in order to deploy built stdlib products to the device. You may
   turn on remote debugging by following the official instructions:
   https://developer.chrome.com/devtools/docs/remote-debugging.

## Part One: "Hello, world" on Android

### 1. Downloading (or building) the Swift Android stdlib dependencies

You may have noticed that, in order to build the Swift stdlib for Linux, you
needed to `apt-get install libicu-dev icu-devtools`. Similarly, building
the Swift stdlib for Android requires the libiconv and libicu libraries.
However, you'll need versions of these libraries that work on Android devices.

You may download prebuilt copies of these dependencies, built for Ubuntu 15.10
and Android NDK r13. Click [here](https://github.com/SwiftAndroid/libiconv-libicu-android/releases/download/android-ndk-r13/libiconv-libicu-armeabi-v7a-ubuntu-15.10-ndk-r13.tar.gz)
to download, then unzip the archive file.

Alternatively, you may choose to build libiconv and libicu for Android yourself.
If you are using Ubuntu 16.04, it is suggested that you build them yourself, as the prebuilt 15.10 copies have not been tested on 16.04.
The steps are as follows:

1. Ensure you have `curl`, `autoconf`, `automake`, `libtool`, and
   `git` installed.
2. Clone the [SwiftAndroid/libiconv-libicu-android](https://github.com/SwiftAndroid/libiconv-libicu-android)
   project. From the command-line, run the following command:
   `git clone https://github.com/SwiftAndroid/libiconv-libicu-android.git`.
3. From the command-line, run `which ndk-build`. Confirm that the path to
   the `ndk-build` executable in the Android NDK you downloaded is displayed.
   If not, you may need to add the Android NDK directory to your `PATH`.
4. Enter the `libiconv-libicu-android` directory on the command line, then
   run `build.sh`.
5. Confirm that the build script created `armeabi-v7a/icu/source/i18n` and
   `armeabi-v7a/icu/source/common` directories within your
   `libiconv-libicu-android` directory.

### 2. Building the Swift stdlib for Android

Enter your Swift directory, then run the build script, passing paths to the
Android NDK, as well as the directories that contain the `libicuuc.so` and
`libicui18n.so` you downloaded or built in step one:

```
$ utils/build-script \
    -R \                                       # Build in ReleaseAssert mode.
    --android \                                # Build for Android.
    --android-ndk /path/to/android-ndk-r14 \   # Path to an Android NDK.
    --android-api-level 21 \                   # The Android API level to target. Swift only supports 21 or greater.
    --android-icu-uc /path/to/libicu-android/armeabi-v7a \
    --android-icu-uc-include /path/to/libicu-android/armeabi-v7a/icu/source/common \
    --android-icu-i18n /path/to/libicu-android/armeabi-v7a \
    --android-icu-i18n-include /path/to/libicu-android/armeabi-v7a/icu/source/i18n/
```

### 3. Compiling `hello.swift` to run on an Android device

Create a simple Swift file named `hello.swift`:

```swift
print("Hello, Android")
```

To compile it, we need to make sure the correct linker is used. Symlink the
gold linker in the Android NDK into your `PATH`:

```
$ sudo ln -s \
    /path/to/android-ndk-r14/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/arm-linux-androideabi/bin/ld.gold \
    /usr/bin/armv7-none-linux-androideabi-ld.gold
```

Then use the built Swift compiler from the previous step to compile a Swift
source file, targeting Android:

```
$ build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swiftc \                      # The Swift compiler built in the previous step.
                                                                                 # The location of the tools used to build Android binaries
    -tools-directory /path/to/android-ndk-r14/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/arm-linux-androideabi/bin
    -target armv7-none-linux-androideabi \                                       # Targeting android-armv7.
    -sdk /path/to/android-ndk-r14/platforms/android-21/arch-arm \                # Use the same NDK path and API version as you used to build the stdlib in the previous step.
    -L /path/to/android-ndk-r14/sources/cxx-stl/llvm-libc++/libs/armeabi-v7a \   # Link the Android NDK's libc++ and libgcc.
    -L /path/to/android-ndk-r14/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/lib/gcc/arm-linux-androideabi/4.9 \
    hello.swift
```

This should produce a `hello` executable in the directory you executed the
command. If you attempt to run this executable using your Linux environment,
you'll see the following error:

```
cannot execute binary file: Exec format error
```

This is exactly the error we want: the executable is built to run on an
Android device--it does not run on Linux. Next, let's deploy it to an Android
device in order to execute it.

### 4. Deploying the build products to the device

You can use the `adb push` command to copy build products from your Linux
environment to your Android device. If you haven't already installed `adb`,
you may do so via `apt-get`:

```
$ sudo apt-get install android-tools-adb
```

Once you have `adb` installed, verify your device is connected and is
listed when you run the `adb devices` command, then run the following
commands to copy the Swift Android stdlib:

```
$ adb push build/Ninja-ReleaseAssert/swift-linux-x86_64/lib/swift/android/libswiftCore.so /data/local/tmp
$ adb push build/Ninja-ReleaseAssert/swift-linux-x86_64/lib/swift/android/libswiftGlibc.so /data/local/tmp
$ adb push build/Ninja-ReleaseAssert/swift-linux-x86_64/lib/swift/android/libswiftSwiftOnoneSupport.so /data/local/tmp
$ adb push build/Ninja-ReleaseAssert/swift-linux-x86_64/lib/swift/android/libswiftRemoteMirror.so /data/local/tmp
$ adb push build/Ninja-ReleaseAssert/swift-linux-x86_64/lib/swift/android/libswiftSwiftExperimental.so /data/local/tmp
```

In addition, you'll also need to copy the Android NDK's libc++:

```
$ adb push /path/to/android-ndk-r14/sources/cxx-stl/llvm-libc++/libs/armeabi-v7a/libc++_shared.so /data/local/tmp
```

Finally, you'll need to copy the `hello` executable you built in the
previous step:
```
$ adb push hello /data/local/tmp
```

### 5. Running "Hello, world" on your Android device

You can use the `adb shell` command to execute the `hello` executable on
the Android device:

```
$ adb shell LD_LIBRARY_PATH=/data/local/tmp /data/local/tmp/hello
```

You should see the following output:

```
Hello, Android
```

Congratulations! You've just run your first Swift program on Android.

## Part Two: Running the Swift test suite hosted on an Android device

When running the test suite, build products are automatically pushed to your
device. As in part one, you'll need to connect your Android device via USB:

1. Connect your Android device to your computer via USB. Ensure that remote
   debugging is enabled for that device by following the official instructions:
   https://developer.chrome.com/devtools/docs/remote-debugging.
2. Confirm the device is connected by running `adb devices`. You should see
   your device listed.
3. Run the tests using the build script:

```
$ utils/build-script \
  -R \                                           # Build in ReleaseAssert mode.
  -T \                                           # Run all tests.
  --android \                                    # Build for Android.
  --android-ndk ~/android-ndk-r13 \              # Path to an Android NDK.
  --android-ndk-version 21 \
  --android-icu-uc ~/libicu-android/armeabi-v7a/libicuuc.so \
  --android-icu-uc-include ~/libicu-android/armeabi-v7a/icu/source/common \
  --android-icu-i18n ~/libicu-android/armeabi-v7a/libicui18n.so \
  --android-icu-i18n-include ~/libicu-android/armeabi-v7a/icu/source/i18n/
```
