# Building Swift on Windows

Visual Studio 2017 or newer is needed to build swift on Windows.

## 1. Install dependencies
- Install the latest version of [Visual Studio](https://www.visualstudio.com/downloads/)
- Make sure to include "Programming Languages|Visual C++" and "Windows and Web
  Development|Universal Windows App Development|Windows SDK" in your
  installation.

## 2. Clone the repositories
1. Configure git to work with Unix file endings
1. Create a folder to contain all the Swift repositories
1. Clone `apple/swift-cmark` into a folder named `cmark`
1. Clone `apple/swift-clang` into a folder named `clang`
1. Clone `apple/swift-llvm` into a folder named `llvm`
1. Clone `apple/swift-compiler-rt` into a folder named `compiler-rt`
1. Clone `apple/swift` into a folder named `swift`
1. Clone `apple/swift-corelibs-libdispatch` into a folder named `swift-corelibs-libdispatch`
1. Clone `apple/swift-corelibs-foundation` into a folder named `swift-corelibs-foundation`
1. Clone `apple/swift-corelibs-xctest` into a folder name `swift-corelibs-xctest`
1. Clone `apple/swift-lldb` into a folder named `lldb`
1. Clone `apple/swift-llbuild` into a folder named `llbuild`
1. Clone `apple/swift-package-manager` info a folder named `swift-package-manager`
1. Clone `curl` into a folder named `curl`
1. Clone `libxml2` into a folder named `libxml2`

- Currently, other repositories in the Swift project have not been tested and
  may not be supported.

This guide assumes your sources live at the root of `S:`. If your sources live elsewhere, you can create a substitution for this:

```cmd
subst S: <path to sources>
```

```cmd
S:
git clone https://github.com/apple/swift-cmark cmark
git clone https://github.com/apple/swift-clang clang
git clone https://github.com/apple/swift-llvm llvm
git clone https://github.com/apple/swift-compiler-rt compiler-rt
git clone -c core.autocrlf=input -c core.symlinks=true https://github.com/apple/swift
git clone https://github.com/apple/swift-corelibs-libdispatch
git clone https://github.com/apple/swift-corelibs-foundation
git clone https://github.com/apple/swift-corelibs-xctest
git clone https://github.com/apple/swift-lldb lldb
git clone https://github.com/apple/swift-llbuild llbuild
git clone -c core.autocrlf=input https://github.com/apple/swift-package-manager
git clone https://github.com/curl/curl.git
git clone https://gitlab.gnome.org/GNOME/libxml2.git
```

## 3. Acquire ICU
1. Download ICU from [ICU Project](http://site.icu-project.org) for Windows x64 and extract the folder to a new folder called `thirdparty`. In other words, there should be a folder `S:\thirdparty\icu4c-64_2-Win64-MSVC2017` with the ICU. 
1. Add the `bin64` folder to your `Path` environment variable.

```cmd
PATH S:\thirdparty\icu4c-64_2-Win64-MSVC2017\bin64;%PATH%
```

## 4. Fetch SQLite3

```powershell
(New-Object System.Net.WebClient).DownloadFile("https://www.sqlite.org/2019/sqlite-amalgamation-3270200.zip", "S:\sqlite-amalgamation-3270200.zip")
Add-Type -A System.IO.Compression.FileSystem
[IO.Compression.ZipFile]::ExtractToDirectory("S:\sqlite-amalgamation-3270200.zip", "S:\")
```

## 5. Get ready
- From within a **developer** command prompt (not PowerShell nor cmd, but the [Visual Studio Developer Command Prompt](https://msdn.microsoft.com/en-us/library/f35ctcxw.aspx)), execute the following command if you have an x64 PC.

```cmd
VsDevCmd -arch=amd64
```

If instead you're compiling for a 32-bit Windows target, adapt the `arch` argument to `x86` and run

```cmd
VsDevCmd -arch=x86
```

- Decide whether you want to build a release or debug version of Swift on Windows and 
  replace the `CMAKE_BUILD_TYPE` parameter in the build steps below with the correct value 
  (`Debug`, `RelWithDebInfoAssert` or `Release`) to avoid conflicts between the debug and 
  non-debug version of the MSCRT library.

- Set up the `ucrt`, `visualc`, and `WinSDK` modules by copying  `ucrt.modulemap` located at
  `swift/stdlib/public/Platform/ucrt.modulemap` into
  `${UniversalCRTSdkDir}/Include/${UCRTVersion}/ucrt` as `module.modulemap`, copying `visualc.modulemap` located at `swift/stdlib/public/Platform/visualc.modulemap` into `${VCToolsInstallDir}/include` as `module.modulemap`, and copying `winsdk.modulemap` located at `swift/stdlib/public/Platform/winsdk.modulemap` into `${UniversalCRTSdkDir}/Include/${UCRTVersion}/um` and setup the `visualc.apinotes` located at `swift/stdlib/public/Platform/visualc.apinotes` into `${VCToolsInstallDir}/include` as `visualc.apinotes`

```cmd
mklink "%UniversalCRTSdkDir%\Include\%UCRTVersion%\ucrt\module.modulemap" S:\swift\stdlib\public\Platform\ucrt.modulemap
mklink "%UniversalCRTSdkDir%\Include\%UCRTVersion%\um\module.modulemap" S:\swift\stdlib\public\Platform\winsdk.modulemap
mklink "%VCToolsInstallDir%\include\module.modulemap" S:\swift\stdlib\public\Platform\visualc.modulemap
mklink "%VCToolsInstallDir%\include\visualc.apinotes" S:\swift\stdlib\public\Platform\visualc.apinotes
```

Warning: Creating the above links usually requires administrator privileges. The quick and easy way to do this is to open a second developer prompt by right clicking whatever shortcut you used to open the first one, choosing Run As Administrator, and pasting the above commands into the resulting window. You can then close the privileged prompt; this is the only step which requires elevation.

## 6. Build LLVM/Clang
- This must be done from within a developer command prompt. LLVM and Clang are
  large projects, so building might take a few hours. Make sure that the build
  type for LLVM/Clang is compatible with the build type for Swift. That is,
  either build everything `Debug` or some variant of `Release` (e.g. `Release`,
  `RelWithDebInfo`).
```cmd
md "S:\b\llvm"
cd "S:\b\llvm"
cmake -G Ninja^
 -DCMAKE_BUILD_TYPE=Release^
 -DCMAKE_C_COMPILER=cl^
 -DCMAKE_CXX_COMPILER=cl^
 -DLLVM_DEFAULT_TARGET_TRIPLE=x86_64-unknown-windows-msvc^
 -DLLVM_ENABLE_ASSERTIONS=ON^
 -DLLVM_ENABLE_PDB=YES^
 -DLLVM_ENABLE_PROJECTS=clang^
 -DLLVM_TARGETS_TO_BUILD="AArch64;ARM;X86"^
 S:/llvm
ninja
```

- Update your path to include the LLVM tools.

```cmd
path S:\b\llvm\bin;%PATH%
```
## 7. Build CMark
- This must be done from within a developer command prompt. CMark is a fairly
  small project and should only take a few minutes to build.
```cmd
md "S:\b\cmark"
cd "S:\b\cmark"
cmake -G Ninja^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo^
  -DCMAKE_C_COMPILER=cl^
  -DCMAKE_CXX_COMPILER=cl^
  S:\cmark
ninja
```

## 8. Build Swift
- This must be done from within a developer command prompt
- Note that Visual Studio vends a 32-bit python 2.7 installation in `C:\Python27` and a 64-bit python in `C:\Python27amd64`.  You may use either one based on your installation.

```cmd
md "S:\b\swift"
cd "S:\b\swift"
cmake -G Ninja^
 -DCMAKE_BUILD_TYPE=RelWithDebInfo^
 -DCMAKE_C_COMPILER=cl^
 -DCMAKE_CXX_COMPILER=cl^
 -DCMAKE_EXE_LINKER_FLAGS:STRING="/INCREMENTAL:NO"^
 -DCMAKE_SHARED_LINKER_FLAGS:STRING="/INCREMENTAL:NO"^
 -DSWIFT_INCLUDE_DOCS=OFF^
 -DSWIFT_PATH_TO_CMARK_SOURCE="S:\cmark"^
 -DSWIFT_PATH_TO_CMARK_BUILD="S:\b\cmark"^
 -DLLVM_DIR=S:\b\llvm\lib\cmake\llvm^
 -DClang_DIR=S:\b\llvm\lib\cmake\clang^
 -DSWIFT_PATH_TO_LIBDISPATCH_SOURCE="S:\swift-corelibs-libdispatch"^
 -DSWIFT_WINDOWS_x86_64_ICU_UC_INCLUDE="S:/thirdparty/icu4c-64_2-Win64-MSVC2017/include"^
 -DSWIFT_WINDOWS_x86_64_ICU_UC="S:/thirdparty/icu4c-64_2-Win64-MSVC2017/lib64/icuuc.lib"^
 -DSWIFT_WINDOWS_x86_64_ICU_I18N_INCLUDE="S:/thirdparty/icu4c-64_2-Win64-MSVC2017/include"^
 -DSWIFT_WINDOWS_x86_64_ICU_I18N="S:/thirdparty/icu4c-64_2-Win64-MSVC2017/lib64/icuin.lib"^
 -DCMAKE_INSTALL_PREFIX="C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr"^
 -DPYTHON_EXECUTABLE=C:\Python27\python.exe^
 S:\swift
ninja
```

- To create a Visual Studio project, you'll need to change the generator and,
  if you have a 64 bit processor, specify the generator platform. Note that you
  may get multiple build errors compiling the `swift` project due to an MSBuild
  limitation that file paths cannot exceed 260 characters. These can be
  ignored, as they occur after the build when writing the last build status to
  a file.

```cmd
cmake -G "Visual Studio 2017" -A x64 -T "host=x64"^ ...
```

## 9. Build lldb
- This must be done from within a developer command prompt and could take hours
  depending on your system.
```cmd
md "S:\b\lldb"
cd "S:\b\lldb"
cmake -G Ninja^
  -DLLVM_DIR="S:/b/llvm/lib/cmake/llvm"^
  -DClang_DIR="S:/b/llvm/lib/cmake/clang"^
  -DSwift_DIR="S:/b/swift/lib/cmake/swift"^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo^
  -DLLDB_ALLOW_STATIC_BINDINGS=YES^
  -DLLVM_ENABLE_ASSERTIONS=ON^
  -DPYTHON_HOME="%ProgramFiles(x86)%\Microsoft Visual Studio\Shared\Python37_64"^
  S:\lldb
ninja
```

## 10. Running tests on Windows

Running the testsuite on Windows has additional external dependencies.

```cmd
path S:\thirdparty\icu4c-64_2-Win64-MSVC2017\bin64;S:\b\swift\bin;S:\b\swift\libdispatch-prefix\bin;%PATH%;%ProgramFiles%\Git\usr\bin
ninja -C S:\b\swift check-swift
```

## 11. Build swift-corelibs-libdispatch

```cmd
md "S:\b\libdispatch"
cd "S:\b\libdispatch"
cmake -G Ninja^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo^
  -DCMAKE_C_COMPILER=clang-cl^
  -DCMAKE_CXX_COMPILER=clang-cl^
  -DCMAKE_SWIFT_COMPILER=S:\b\swift\bin\swiftc.exe^
  -DENABLE_SWIFT=ON^
  -DENABLE_TESTING=OFF^
  S:\swift-corelibs-libdispatch
ninja
```

- Add libdispatch to your path:
```cmd
path S:\b\libdispatch;S:\b\libdispatch\src;%PATH%
```

## 12. Build curl

```cmd
cd "S:\curl"
.\buildconf.bat
cd winbuild
nmake /f Makefile.vc mode=static VC=15 MACHINE=x64
```

## 13. Build libxml2

```cmd
cd "S:\libxml2\win32"
cscript //E:jscript configure.js iconv=no
nmake /f Makefile.msvc
```

## 14. Build swift-corelibs-foundation

```cmd
md "S:\b\foundation"
cd "S:\b\foundation
cmake -G Ninja^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo^
  -DCMAKE_C_COMPILER=clang-cl^
  -DCMAKE_SWIFT_COMPILER=S:\b\swift\bin\swiftc.exe^
  -DCURL_LIBRARY="S:/curl/builds/libcurl-vc15-x64-release-static-ipv6-sspi-winssl/lib/libcurl_a.lib"^
  -DCURL_INCLUDE_DIR="S:/curl/builds/libcurl-vc15-x64-release-static-ipv6-sspi-winssl/include"^
  -DENABLE_TESTING=NO^
  -DICU_ROOT="S:/thirdparty/icu4c-64_2-Win64-MSVC2017"^
  -DLIBXML2_LIBRARY="S:/libxml2/win32/bin.msvc/libxml2_a.lib"^
  -DLIBXML2_INCLUDE_DIR="S:/libxml2/include"^
  -DFOUNDATION_PATH_TO_LIBDISPATCH_SOURCE=S:\swift-corelibs-libdispatch^
  -DFOUNDATION_PATH_TO_LIBDISPATCH_BUILD=S:\b\libdispatch^
   S:\swift-corelibs-foundation
ninja
```

- Add Foundation to your path:
```cmd
path S:\b\foundation;%PATH%
```

## 15. Build swift-corelibs-xctest

```cmd
md "S:\b\xctest"
cd "S:\b\xctest"
cmake -G Ninja^
  -DBUILD_SHARED_LIBS=YES^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo^
  -DCMAKE_SWIFT_COMPILER=S:\b\swift\bin\swiftc.exe^
  -DXCTEST_PATH_TO_FOUNDATION_BUILD=S:\b\foundation^
  -DXCTEST_PATH_TO_LIBDISPATCH_SOURCE=S:\swift-corelibs-libdispatch^
  -DXCTEST_PATH_TO_LIBDISPATCH_BUILD=S:\b\libdispatch^
  -DLIT_COMMAND=S:\llvm\utils\lit\lit.py^
  -DPYTHON_EXECUTABLE=C:\Python27\python.exe^
  S:\swift-corelibs-xctest
ninja
```

- Add XCTest to your path:
```cmd
path S:\b\xctest;%PATH%
```

## 16. Test XCTest

```cmd
ninja -C S:\b\xctest check-xctest
```

## 17. Rebuild Foundation

```cmd
cd "S:\b\foundation
cmake -G Ninja^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo^
  -DCMAKE_C_COMPILER=clang-cl^
  -DCMAKE_SWIFT_COMPILER=S:\b\swift\bin\swiftc.exe^
  -DCURL_LIBRARY="S:/curl/builds/libcurl-vc15-x64-release-static-ipv6-sspi-winssl/lib/libcurl_a.lib"^
  -DCURL_INCLUDE_DIR="S:/curl/builds/libcurl-vc15-x64-release-static-ipv6-sspi-winssl/include"^
  -DENABLE_TESTING=YES^
  -DICU_ROOT="S:/thirdparty/icu4c-64_2-Win64-MSVC2017"^
  -DLIBXML2_LIBRARY="S:/libxml2/win32/bin.msvc/libxml2_a.lib"^
  -DLIBXML2_INCLUDE_DIR="S:/libxml2/include"^
  -DFOUNDATION_PATH_TO_LIBDISPATCH_SOURCE=S:\swift-corelibs-libdispatch^
  -DFOUNDATION_PATH_TO_LIBDISPATCH_BUILD=S:\b\libdispatch^
  -DFOUNDATION_PATH_TO_XCTEST_BUILD=S:\b\xctest^
   S:\swift-corelibs-foundation
ninja
```

## 18. Test Foundation

```cmd
cmake --build S:\b\foundation
ninja -C S:\b\foundation test 
```

## 19. Build SQLite3

```cmd
md S:\b\sqlite
cd S:\b\sqlite
cl /MD /Ox /Zi /LD /DSQLITE_API=__declspec(dllexport) S:\sqlite-amalgamation-3270200\sqlite3.c
```

 - Add SQLite3 to your path:
```cmd
path S:\b\sqlite;%PATH%
```

## 20. Build llbuild

```cmd
md S:\b\llbuild
cd S:\b\llbuild
set AR=llvm-ar
cmake -G Ninja^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo^
  -DCMAKE_C_COMPILER=cl^
  -DCMAKE_CXX_COMPILER=cl^
  -DFOUNDATION_BUILD_DIR=S:\b\foundation^
  -DLIBDISPATCH_BUILD_DIR=S:\b\libdispatch^
  -DLIBDISPATCH_SOURCE_DIR=S:\swift-corelibs-libdispatch^
  -DSQLite3_INCLUDE_DIR=S:\sqlite-amalgamation-3270200^
  -DSQLite3_LIBRARY=S:\b\sqlite\sqlite3.lib^
  -DLLBUILD_SUPPORT_BINDINGS=Swift^
  S:\llbuild
ninja
```

 - Add llbuild to your path:
```cmd
path S:\b\llbuild\bin;%PATH%
```

## 21. Build swift-package-manager

```cmd
md S:\b\spm
cd S:\b\spm
C:\Python27\python.exe S:\swift-package-manager\Utilities\bootstrap --foundation S:\b\foundation --libdispatch-build-dir S:\b\libdispatch --libdispatch-source-dir S:\swift-corelibs-libdispatch --llbuild-build-dir S:\b\llbuild --llbuild-source-dir S:\llbuild --sqlite-build-dir S:\b\sqlite --sqlite-source-dir S:\sqlite-amalgamation-3270200
```

## 22. Install Swift on Windows

- Run ninja install:

```cmd 
ninja -C S:\b\swift install
```

- Add the Swift on Windows binaries path (`C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr\bin`)  to the `PATH` environment variable.

## Resuming Builds

If you resume development from a new shell, the path will need to be readjusted.  The following will add the correct search order to the path:

```cmd
path S:\thirdparty\icu4c-63_1-Win64-MSVC2017\bin64;S:\b\llvm\bin;S:\b\swift\bin;S:\b\libdispatch;S:\b\libdispatch\src;S:\b\foundation;S:\b\xctest;S:\b\llbuild\bin;S:\b\sqlite;%PATH%;%ProgramFiles%\Git\usr\bin
```
