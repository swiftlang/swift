# Cross-compiling Swift for Windows with `clang`

This document describes how to cross compile Swift for Windows on a non-Windows
host. For more context on the status of Swift on Windows in general, see
[Getting Started with Swift on Windows](./Windows.md)

## 1. Clone

## Get WinSDK

## ICU (same as windoge build)

## 1. Set up Visual Studio environment variables
Building for Windows requires that the Visual Studio environment variables are
setup similar to the values on Windows. Currently, the runtime has been tested
to build against the Windows 10 SDK at revision 10.10.586.

```bash
# Visual Studio 2015 does not have VCToolsInstallDir, use VCINSTALLDIR's value
export UCRTVersion=10.0.10586.0 // TODO(asb): the versions used for the next 3 lines
export UniversalCRTSdkDir=".../Windows Kits/10"
export VCToolsInstallDir=".../Microsoft Visual Studio/2017/Community"
```


## 2. Get ICU
Find out where to download source to compile this binary: https://www.npcglib.org/~stathis/blog/precompiled-icu/

## 2. Set up the `ucrt`, `visualc`, and `WinSDK` modules

download the WinSDK from https://developer.microsoft.com/en-us/windows/downloads/windows-10-sdk (get the ISO)

```
mkdir -p ${UniversalCRTSdkDir}/Include/${UCRTVersion}/ucrt
ln -s /home/ubuntu/swift-source/swift/stdlib/public/Platform/ucrt.modulemap ${UniversalCRTSdkDir}/Include/${UCRTVersion}/ucrt/module.modulemap

mkdir -p ${VCToolsInstallDir}/include
ln -s /home/ubuntu/swift-source/swift/stdlib/public/Platform/visualc.modulemap ${VCToolsInstallDir}/include/module.modulemap

mkdir -p ${UniversalCRTSdkDir}/Include/10.0.107663/um
ln -s /home/ubuntu/swift-source/swift/stdlib/public/Platform/winsdk.modulemap ${UniversalCRTSdkDir}/Include/10.0.107663/um/module.modulemap
```

- Set up the `ucrt`, `visualc`, and `WinSDK` modules by copying  `ucrt.modulemap` located at
  `swift/stdlib/public/Platform/ucrt.modulemap` into
  `${UniversalCRTSdkDir}/Include/${UCRTVersion}/ucrt` as 
  `module.modulemap`, copying `visualc.modulemap` located at `swift/stdlib/public/Platform/visualc.modulemap` into `${VCToolsInstallDir}/include` as `module.modulemap`, and copying `winsdk.modulemap` located at `swift/stdlib/public/Platform/winsdk.modulemap` into `${UniversalCRTSdkDir}/Include/10.0.107663/um`


## 3. Configure the runtime to be built with the just built `clang`
Ensure that we use the tools from the just built LLVM and `clang` tools to 
build the Windows SDK. You will need to pass a few extra options to cmake via
the `build-script` invocation to achieve this. You will need to expand out the
path where `llvm-ar` and `llvm-ranlib` are built. These are needed to correctly
build the static libraries. Note that cross-compiling will require the use of
`lld`. Ensure that `lld-link.exe` is available to clang via your path.
Additionally, the ICU headers and libraries need to be provided for the build.

#### macOS

```bash
--extra-cmake-options=-DSWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER=FALSE,\
-DCMAKE_AR=<path to llvm-ar>,\
-DCMAKE_RANLIB=<path to llvm-ranlib>,\
-DSWIFT_SDKS='OSX;WINDOWS',\
-DSWIFT_WINDOWS_x86_64_ICU_I18N_INCLUDE=<path to ICU i18n includes>,\
-DSWIFT_WINDOWS_x86_64_ICU_UC_INCLUDE=<path to ICU UC includes>,\
-DSWIFT_WINDOWS_x86_64_ICU_I18N=<path to ICU i18n lib>,\
-DSWIFT_WINDOWS_x86_64_ICU_UC=<path to ICU UC lib>
```

#### Linux

For Linux, you will need to build the Linux SDK instead of the macOS SDK by
replacing the cmake option with `-DSWIFT_SDKS='LINUX;WINDOWS'`.

## 4. Build the Swift runtime and standard library with `ninja`
From the build directory, you can build the Swift runtime and standard library
for Windows using `ninja swiftCore-windows-x86_64`.
