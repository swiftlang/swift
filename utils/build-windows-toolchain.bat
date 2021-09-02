:: build-windows-toolchain.bat
::
:: This source file is part of the Swift.org open source project
::
:: Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
:: Licensed under Apache License v2.0 with Runtime Library Exception
::
:: See https://swift.org/LICENSE.txt for license information
:: See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

setlocal enableextensions enabledelayedexpansion
path %PATH%;%PYTHON_HOME%

:: Identify the SourceRoot
:: Normalize the SourceRoot to make it easier to read the output.
cd %~dp0\..\..
set SourceRoot=%CD%

:: Identify the BuildRoot
set BuildRoot=%SourceRoot%\build

md %BuildRoot%
subst T: /d
subst T: %BuildRoot% || (exit /b)
set BuildRoot=T:

:: Identify the PackageRoot
set PackageRoot=%BuildRoot%\package

md %PackageRoot%

:: Identify the InstallRoot
set InstallRoot=%BuildRoot%\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr
set PlatformRoot=%BuildRoot%\Library\Developer\Platforms\Windows.platform
set SDKInstallRoot=%PlatformRoot%\Developer\SDKs\Windows.sdk

:: Setup temporary directories
md %BuildRoot%\tmp
set TEMP=%BuildRoot%\tmp
set TMP=%BuildRoot%\tmp
set TMPDIR=%BuildRoot%\tmp

call :CloneDependencies || (exit /b)
call :CloneRepositories || (exit /b)

md "%BuildRoot%\Library"

:: TODO(compnerd) build ICU from source
curl.exe -sOL "https://github.com/unicode-org/icu/releases/download/release-67-1/icu4c-67_1-Win64-MSVC2017.zip" || (exit /b)
"%SystemDrive%\Program Files\Git\usr\bin\unzip.exe" -o icu4c-67_1-Win64-MSVC2017.zip -d %BuildRoot%\Library\icu-67.1
md %BuildRoot%\Library\icu-67.1\usr\bin
copy %BuildRoot%\Library\icu-67.1\bin64\icudt67.dll %BuildRoot%\Library\icu-67.1\usr\bin || (exit /b)
copy %BuildRoot%\Library\icu-67.1\bin64\icuin67.dll %BuildRoot%\Library\icu-67.1\usr\bin || (exit /b)
copy %BuildRoot%\Library\icu-67.1\bin64\icuuc67.dll %BuildRoot%\Library\icu-67.1\usr\bin || (exit /b)

:: FIXME(compnerd) is there a way to build the sources without downloading the amalgamation?
curl.exe -sOL "https://sqlite.org/2021/sqlite-amalgamation-3360000.zip" || (exit /b)
"%SystemDrive%\Program Files\Git\usr\bin\unzip.exe" -o sqlite-amalgamation-3360000.zip -d %SourceRoot%

:: TODO(compnerd) use CMakeLists.txt from compnerd/swift-build
md %BuildRoot%\sqlite
cl /nologo /DWIN32 /D_WINDOWS /W3 /MD /O2 /Ob2 /DNDEBUG /Fo%BuildRoot%\sqlite\sqlite3.c.obj /Fd%BuildRoot%\sqlite\SQLite3.pdb /FS -c %SourceRoot%\sqlite-amalgamation-3360000\sqlite3.c
lib /nologo /machine:x64 /out:%BuildRoot%\sqlite\SQLite3.lib %BuildRoot%\sqlite\sqlite3.c.obj
md %BuildRoot%\Library\sqlite-3.36.0\usr\lib
md %BuildRoot%\Library\sqlite-3.36.0\usr\include
copy %BuildRoot%\sqlite\SQLite3.lib %BuildRoot%\Library\sqlite-3.36.0\usr\lib
copy %SourceRoot%\sqlite-amalgamation-3360000\sqlite3.h %BuildRoot%\Library\sqlite-3.36.0\usr\include
copy %SourceRoot%\sqlite-amalgamation-3360000\sqlite3ext.h %BuildRoot%\Library\sqlite-3.36.0\usr\include

:: build zlib
cmake ^
  -B %BuildRoot%\zlib ^

  -D BUILD_SHARED_LIBS=NO ^
  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=cl ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%BuildRoot%\Library\zlib-1.2.11\usr ^

  -D SKIP_INSTALL_FILES=YES ^

  -G Ninja ^
  -S %SourceRoot%\zlib || (exit /b)
cmake --build "%BUildRoot%\zlib" || (exit /b)
cmake --build "%BUildRoot%\zlib" --target install || (exit /b)

:: build libxml2
cmake ^
  -B %BuildRoot%\libxml2 ^

  -D BUILD_SHARED_LIBS=OFF ^
  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=cl ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%BuildRoot%\Library\libxml2-2.9.12\usr ^

  -D LIBXML2_WITH_ICONV=NO ^
  -D LIBXML2_WITH_ICU=NO ^
  -D LIBXML2_WITH_LZMA=NO ^
  -D LIBXML2_WITH_PYTHON=NO ^
  -D LIBXML2_WITH_TESTS=NO ^
  -D LIBXML2_WITH_THREADS=YES ^
  -D LIBXML2_WITH_ZLIB=NO ^

  -G Ninja ^
  -S %SourceRoot%\libxml2 || (exit /b)
cmake --build "%BUildRoot%\libxml2" || (exit /b)
cmake --build "%BUildRoot%\libxml2" --target install || (exit /b)

:: build curl
cmake ^
  -B %BuildRoot%\curl ^

  -D BUILD_SHARED_LIBS=NO ^
  -D BUILD_TESTING=NO ^
  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=cl ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%BuildRoot%\Library\curl-7.77.0\usr ^

  -D BUILD_CURL_EXE=NO ^
  -D CMAKE_USE_OPENSSL=NO ^
  -D CURL_CA_PATH=none ^
  -D CMAKE_USE_SCHANNEL=YES ^
  -D CMAKE_USE_LIBSSH2=NO ^
  -D HAVE_POLL_FINE=NO ^
  -D CURL_DISABLE_LDAP=YES ^
  -D CURL_DISABLE_LDAPS=YES ^
  -D CURL_DISABLE_TELNET=YES ^
  -D CURL_DISABLE_DICT=YES ^
  -D CURL_DISABLE_FILE=YES ^
  -D CURL_DISABLE_TFTP=YES ^
  -D CURL_DISABLE_RTSP=YES ^
  -D CURL_DISABLE_PROXY=YES ^
  -D CURL_DISABLE_POP3=YES ^
  -D CURL_DISABLE_IMAP=YES ^
  -D CURL_DISABLE_SMTP=YES ^
  -D CURL_DISABLE_GOPHER=YES ^
  -D CURL_ZLIB=YES ^
  -D ENABLE_UNIX_SOCKETS=NO ^
  -D ENABLE_THREADED_RESOLVER=NO ^

  -D ZLIB_ROOT=%BuildRoot%\Library\zlib-1.2.11\usr ^
  -D ZLIB_LIBRARY=%BuildRoot%\Library\zlib-1.2.11\usr\lib\zlibstatic.lib ^

  -G Ninja ^
  -S %SourceRoot%\curl || (exit /b)
cmake --build "%BuildRoot%\curl" || (exit /b)
cmake --build "%BuildRoot%\curl" --target install || (exit /b)

:: Prepare system modules
copy /y "%SourceRoot%\swift\stdlib\public\Platform\ucrt.modulemap" "%UniversalCRTSdkDir%\Include\%UCRTVersion%\ucrt\module.modulemap" || (exit /b)
copy /y "%SourceRoot%\swift\stdlib\public\Platform\winsdk.modulemap" "%UniversalCRTSdkDir%\Include\%UCRTVersion%\um\module.modulemap" || (exit /b)
copy /y "%SourceRoot%\swift\stdlib\public\Platform\visualc.modulemap" "%VCToolsInstallDir%\include\module.modulemap" || (exit /b)
copy /y "%SourceRoot%\swift\stdlib\public\Platform\visualc.apinotes" "%VCToolsInstallDir%\include\visualc.apinotes" || (exit /b)

:: Build Toolchain
cmake ^
  -B "%BuildRoot%\1" ^

  -C %SourceRoot%\swift\cmake\caches\Windows-x86_64.cmake ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=cl ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=cl ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX="%InstallRoot%" ^

  -D LLVM_DEFAULT_TARGET_TRIPLE=x86_64-unknown-windows-msvc ^

  -D PACKAGE_VENDOR="swift.org" ^
  -D CLANG_VENDOR="swift.org" ^
  -D CLANG_VENDOR_UTI="org.swift" ^
  -D LLVM_APPEND_VC_REV=NO ^
  -D LLVM_VERSION_SUFFIX="" ^

  -D SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY=YES ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED=YES ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING=YES ^

  -D LLVM_EXTERNAL_SWIFT_SOURCE_DIR="%SourceRoot%\swift" ^
  -D LLVM_EXTERNAL_CMARK_SOURCE_DIR="%SourceRoot%\cmark" ^
  -D PYTHON_HOME=%PYTHON_HOME% ^
  -D PYTHON_EXECUTABLE=%PYTHON_HOME%\python.exe ^
  -D SWIFT_PATH_TO_LIBDISPATCH_SOURCE="%SourceRoot%\swift-corelibs-libdispatch" ^
  -D SWIFT_WINDOWS_x86_64_ICU_UC_INCLUDE="%BuildRoot%\Library\icu-67.1\include\unicode" ^
  -D SWIFT_WINDOWS_x86_64_ICU_UC="%BuildRoot%\Library\icu-67.1\lib64\icuuc.lib" ^
  -D SWIFT_WINDOWS_x86_64_ICU_I18N_INCLUDE="%BuildRoot%\Library\icu-67.1\include" ^
  -D SWIFT_WINDOWS_x86_64_ICU_I18N="%BuildRoot%\Library\icu-67.1\lib64\icuin.lib" ^

  -G Ninja ^
  -S llvm-project\llvm || (exit /b)
cmake --build "%BuildRoot%\1" || (exit /b)
cmake --build "%BuildRoot%\1" --target install || (exit /b)

:: Build Swift Standard Library
cmake ^
  -B %BuildRoot%\2 ^

  -C %SourceRoot%\swift\cmake\caches\Runtime-Windows-x86_64.cmake ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr ^

  -D LLVM_DIR=%BuildRoot%\1\lib\cmake\llvm ^
  -D SWIFT_NATIVE_SWIFT_TOOLS_PATH=%BuildRoot%\1\bin ^
  -D SWIFT_PATH_TO_LIBDISPATCH_SOURCE=%SourceRoot%\swift-corelibs-libdispatch ^
  -D SWIFT_WINDOWS_x86_64_ICU_UC_INCLUDE="%BuildRoot%\Library\icu-67.1\include\unicode" ^
  -D SWIFT_WINDOWS_x86_64_ICU_UC="%BuildRoot%\Library\icu-67.1\lib64\icuuc.lib" ^
  -D SWIFT_WINDOWS_x86_64_ICU_I18N_INCLUDE="%BuildRoot%\Library\icu-67.1\include" ^
  -D SWIFT_WINDOWS_x86_64_ICU_I18N="%BuildRoot%\Library\icu-67.1\lib64\icuin.lib" ^

  -D SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY=YES ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED=YES ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING=YES ^

  -G Ninja ^
  -S %SourceRoot%\swift || (exit /b)
cmake --build %BuildRoot%\2 || (exit /b)
cmake --build %BuildRoot%\2 --target install || (exit /b)

:: Build libdispatch
cmake ^
  -B %BuildRoot%\3 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr ^

  -D ENABLE_SWIFT=YES ^

  -G Ninja ^
  -S %SourceRoot%\swift-corelibs-libdispatch || (exit /b)
cmake --build %BuildRoot%\3 || (exit /b)
cmake --build %BuildRoot%\3 --target install || (exit /b)

:: Build Foundation
cmake ^
  -B %BuildRoot%\4 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr ^

  -D CURL_DIR=%BuildRoot%\Library\curl-7.77.0\usr\lib\cmake\CURL ^
  -D ICU_ROOT=%BuildRoot%\Library\icu-67.1 ^
  -D ICU_UC_LIBRARY=%BuildRoot%\Library\icu-67.1\lib64\icuuc67.lib ^
  -D ICU_I18N_LIBRARY=%BuildRoot%\Library\icu-67.1\lib64\icuin67.lib ^
  -D LIBXML2_LIBRARY=%BuildRoot%\Library\libxml2-2.9.12\usr\lib\libxml2s.lib ^
  -D LIBXML2_INCLUDE_DIR=%BuildRoot%\Library\libxml2-2.9.12\usr\include\libxml2 ^
  -D LIBXML2_DEFINITIONS="/DLIBXML_STATIC" ^
  -D ZLIB_LIBRARY=%BuildRoot%\Library\zlib-1.2.11\usr\lib\zlibstatic.lib ^
  -D ZLIB_INCLUDE_DIR=%BuildRoot%\Library\zlib-1.2.11\usr\include ^
  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^

  -D ENABLE_TESTING=NO ^

  -G Ninja ^
  -S %SourceRoot%\swift-corelibs-foundation || (exit /b)
cmake --build %BuildRoot%\4 || (exit /b)
cmake --build %BuildRoot%\4 --target install || (exit /b)

:: Build XCTest
cmake ^
  -B %BuildRoot%\5 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%PlatformRoot%\Developer\Library\XCTest-development\usr ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^

  -D ENABLE_TESTING=NO ^

  -G Ninja ^
  -S %SourceRoot%\swift-corelibs-xctest || (exit /b)
cmake --build %BuildRoot%\5 || (exit /b)
cmake --build %BuildRoot%\5 --target install || (exit /b)

:: Build swift-tools-support-core
cmake ^
  -B %BuildRoot%\6 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%InstallRoot% ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^
  -D SQLite3_INCLUDE_DIR=%BuildRoot%\Library\sqlite-3.36.0\usr\include ^
  -D SQLite3_LIBRARY=%BuildRoot%\Library\sqlite-3.36.0\usr\lib\SQLite3.lib ^

  -G Ninja ^
  -S %SourceRoot%\swift-tools-support-core || (exit /b)
cmake --build %BuildRoot%\6 || (exit /b)
cmake --build %BuildRoot%\6 --target install || (exit /b)

:: Build llbuild
cmake ^
  -B %BuildRoot%\7 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy -Xclang -fno-split-cold-code" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%InstallRoot% ^

  -D LLBUILD_SUPPORT_BINDINGS=Swift ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^
  -D SQLite3_INCLUDE_DIR=%BuildRoot%\Library\sqlite-3.36.0\usr\include ^
  -D SQLite3_LIBRARY=%BuildRoot%\Library\sqlite-3.36.0\usr\lib\SQLite3.lib ^

  -G Ninja ^
  -S %SourceRoot%\llbuild || (exit /b)
cmake --build %BuildRoot%\7 || (exit /b)
cmake --build %BuildRoot%\7 --target install || (exit /b)

:: Build swift-argument-parser
cmake ^
  -B %BuildRoot%\8 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%InstallRoot% ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^
  -D XCTest_DIR=%BuildRoot%\5\cmake\modules ^

  -G Ninja ^
  -S %SourceRoot%\swift-argument-parser || (exit /b)
cmake --build %BuildRoot%\8 || (exit /b)
cmake --build %BuildRoot%\8 --target install || (exit /b)

:: Build Yams
cmake ^
  -B %BuildRoot%\9 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy /DYAML_DECLARE_EXPORT /DWIN32" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_Swift_FLAGS="-Xcc -DYAML_DECLARE_EXPORT -Xcc -DWIN32" ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%InstallRoot% ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^
  -D XCTest_DIR=%BuildRoot%\5\cmake\modules ^

  -G Ninja ^
  -S %SourceRoot%\Yams || (exit /b)
cmake --build %BuildRoot%\9 || (exit /b)
cmake --build %BuildRoot%\9 --target install || (exit /b)

:: Build swift-driver
cmake ^
  -B %BuildRoot%\10 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%InstallRoot% ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^
  -D XCTest_DIR=%BuildRoot%\5\cmake\modules ^
  -D TSC_DIR=%BuildRoot%\6\cmake\modules ^
  -D LLBuild_DIR=%BuildRoot%\7\cmake\modules ^
  -D ArgumentParser_DIR=%BuildRoot%\8\cmake\modules ^
  -D Yams_DIR=%BuildRoot%\9\cmake\modules ^

  -G Ninja ^
  -S %SourceRoot%\swift-driver || (exit /b)
cmake --build %BuildRoot%\10 || (exit /b)
cmake --build %BuildRoot%\10 --target install || (exit /b)

:: Build swift-crypto
cmake ^
  -B %BuildRoot%\11 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=cl ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=cl ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%InstallRoot% ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^

  -G Ninja ^
  -S %SourceRoot%\swift-crypto || (exit /b)
cmake --build %BuildRoot%\11 || (exit /b)
cmake --build %BuildRoot%\11 --target install || (exit /b)

:: Build swift-collections
cmake ^
  -B %BuildRoot%\12 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%InstallRoot% ^

  -G Ninja ^
  -S %SourceRoot%\swift-collections || (exit /b)
cmake --build %BuildRoot%\12 || (exit /b)
cmake --build %BuildRoot%\12 --target install || (exit /b)

:: Build swift-package-manager
cmake ^
  -B %BuildRoot%\13 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%InstallRoot% ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^
  -D TSC_DIR=%BuildRoot%\6\cmake\modules ^
  -D LLBuild_DIR=%BuildRoot%\7\cmake\modules ^
  -D ArgumentParser_DIR=%BuildRoot%\8\cmake\modules ^
  -D Yams_DIR=%BuildRoot%\9\cmake\modules ^
  -D SwiftDriver_DIR=%BuildRoot%\10\cmake\modules ^
  -D SwiftCrypto_DIR=%BuildRoot%\11\cmake\modules ^
  -D SwiftCollections_DIR=%BuildRoot%\12\cmake\modules ^

  -G Ninja ^
  -S %SourceRoot%\swiftpm || (exit /b)
cmake --build %BuildRoot%\13 || (exit /b)
cmake --build %BuildRoot%\13 --target install || (exit /b)

:: Build IndexStoreDB
cmake ^
  -B %BuildRoot%\14 ^

  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy -Xclang -fno-split-cold-code" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%InstallRoot% ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^

  -G Ninja ^
  -S %SourceRoot%\indexstore-db || (exit /b)
cmake --build %BuildRoot%\14 || (exit /b)
cmake --build %BuildRoot%\14 --target install || (exit /b)

:: Build SourceKit-LSP
cmake ^
  -B %BuildRoot%\15 ^

  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy -Xclang -fno-split-cold-code" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%InstallRoot% ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^
  -D TSC_DIR=%BuildRoot%\6\cmake\modules ^
  -D LLBuild_DIR=%BuildRoot%\7\cmake\modules ^
  -D ArgumentParser_DIR=%BuildRoot%\8\cmake\modules ^
  -D Yams_DIR=%BuildRoot%\9\cmake\modules ^
  -D SwiftPM_DIR=%BuildRoot%\13\cmake\modules ^
  -D IndexStoreDB_DIR=%BuildRoot%\14\cmake\modules ^
  -D SwiftCollections_DIR=%BuildRoot%\12\cmake\modules ^

  -G Ninja ^
  -S %SourceRoot%\sourcekit-lsp || (exit /b)
cmake --build %BuildRoot%\15 || (exit /b)
cmake --build %BuildRoot%\15 --target install || (exit /b)

:: Create Configuration Files
python -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'DEFAULT_USE_RUNTIME': 'MD' } }), encoding='utf-8'))" > %SDKInstallRoot%\SDKSettings.plist
:: TODO(compnerd) match the XCTest installation name
python -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'XCTEST_VERSION': 'development' } }), encoding='utf-8'))" > %PlatformRoot%\Info.plist

:: Package toolchain.msi
msbuild %SourceRoot%\swift-installer-scripts\platforms\Windows\toolchain.wixproj ^
  -p:RunWixToolsOutOfProc=true ^
  -p:OutputPath=%PackageRoot%\toolchain\ ^
  -p:IntermediateOutputPath=%PackageRoot%\toolchain\ ^
  -p:TOOLCHAIN_ROOT=%BuildRoot%\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain
:: TODO(compnerd) actually perform the code-signing
:: signtool sign /f Apple_CodeSign.pfx /p Apple_CodeSign_Password /tr http://timestamp.digicert.com /fd sha256 %PackageRoot%\toolchain\toolchain.msi

:: Package sdk.msi
msbuild %SourceRoot%\swift-installer-scripts\platforms\Windows\CustomActions\SwiftInstaller\SwiftInstaller.vcxproj -t:restore
msbuild %SourceRoot%\swift-installer-scripts\platforms\Windows\sdk.wixproj ^
  -p:RunWixToolsOutOfProc=true ^
  -p:OutputPath=%PackageRoot%\sdk\ ^
  -p:IntermediateOutputPath=%PackageRoot%\sdk\ ^
  -p:PLATFORM_ROOT=%PlatformRoot%\ ^
  -p:SDK_ROOT=%SDKInstallRoot%\ ^
  -p:SWIFT_SOURCE_DIR=%SourceRoot%\swift\ ^
  -p:PlatformToolset=v142
:: TODO(compnerd) actually perform the code-signing
:: signtool sign /f Apple_CodeSign.pfx /p Apple_CodeSign_Password /tr http://timestamp.digicert.com /fd sha256 %PackageRoot%\sdk\sdk.msi

:: Package runtime.msi
msbuild %SourceRoot%\swift-installer-scripts\platforms\Windows\runtime.wixproj ^
  -p:RunWixToolsOutOfProc=true ^
  -p:OutputPath=%PackageRoot%\runtime\ ^
  -p:IntermediateOutputPath=%PackageRoot%\runtime\ ^
  -p:SDK_ROOT=%SDKInstallRoot%\
:: TODO(compnerd) actually perform the code-signing
:: signtool sign /f Apple_CodeSign.pfx /p Apple_CodeSign_Password /tr http://timestamp.digicert.com /fd sha256 %PackageRoot%\runtime\runtime.msi

:: Package icu.msi
msbuild %SourceRoot%\swift-installer-scripts\platforms\Windows\icu.wixproj ^
  -p:RunWixToolsOutOfProc=true ^
  -p:OutputPath=%PackageRoot%\icu\ ^
  -p:IntermediateOutputPath=%PackageRoot%\icu\ ^
  -p:ProductVersion=67.1 ^
  -p:ProductVersionMajor=67 ^
  -p:ICU_ROOT=%BuildRoot%
:: TODO(compnerd) actually perform the code-signing
:: signtool sign /f Apple_CodeSign.pfx /p Apple_CodeSign_Password /tr http://timestamp.digicert.com /fd sha256 %PackageRoot%\icu\icu.msi

:: Package devtools.msi
msbuild %SourceRoot%\swift-installer-scripts\platforms\Windows\devtools.wixproj ^
  -p:RunWixToolsOutOfProc=true ^
  -p:OutputPath=%PackageRoot%\devtools\ ^
  -p:IntermediateOutputPath=%PackageRoot%\devtools\ ^
  -p:DEVTOOLS_ROOT=%BuildRoot%\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain
:: TODO(compnerd) actually perform the code-signing
:: signtool sign /f Apple_CodeSign.pfx /p Apple_CodeSign_Password /tr http://timestamp.digicert.com /fd sha256 %PackageRoot%\devtools\devtools.msi

:: Collate MSIs
move %PackageRoot%\toolchain\toolchain.msi %PackageRoot% || (exit /b)
move %PackageRoot%\sdk\sdk.msi %PackageRoot% || (exit /b)
move %PackageRoot%\runtime\runtime.msi %PackageRoot% || (exit /b)
move %PackageRoot%\icu\icu.msi %PackageRoot% || (exit /b)
move %PackageRoot%\devtools\devtools.msi %PackageRoot% || (exit /b)

:: Build Installer
msbuild %SourceRoot%\swift-installer-scripts\platforms\Windows\installer.wixproj ^
  -p:RunWixToolsOutOfProc=true ^
  -p:OutputPath=%PackageRoot%\installer\ ^
  -p:IntermediateOutputPath=%PackageRoot%\installer\ ^
  -p:MSI_LOCATION=%PackageRoot%\
:: TODO(compnerd) actually perform the code-signing
:: signtool sign /f Apple_CodeSign.pfx /p Apple_CodeSign_Password /tr http://timestamp.digicert.com /fd sha256 %PackageRoot%\installer\installer.exe

:: Stage Artifacts
md %BuildRoot%\artifacts
:: FIXME(compnerd) should we provide SDKs as standalone artifact?
move %PackageRoot%\sdk.msi %BuildRoot%\artifacts || (exit /b)
:: Redistributable libraries for developers
move %PackageRoot%\runtime.msi %BuildRoot%\artifacts || (exit /b)
move %PackageRoot%\icu.msi %BuildRoot%\artifacts || (exit /b)
:: Installer
move %PackageRoot%\installer\installer.exe %BuildRoot%\artifacts || (exit /b)

:: TODO(compnerd) test LLVM

:: Test Swift
:: TODO(compnerd) make lit adjust the path properly
path %BuildRoot%\3;%BuildRoot%\1\bin;%BuildRoot%\Library\icu-67.1\usr\bin;%PATH%;%SystemDrive%\Program Files\Git\usr\bin
cmake --build %BuildRoot%\1 --target check-swift || (exit /b)

:: Test dispatch
cmake --build %BuildRoot%\3 --target ExperimentalTest || (exit /b)

:: NOTE(compnerd) update the path *before* the build because the tests are
:: executed to shard the test suite.
path %BuildRoot%\5;%BuildRoot%\4\bin;%PATH%

:: Rebuild Foundation (w/ testing)
cmake ^
  -B %BuildRoot%\4 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr ^

  -D CURL_DIR=%BuildRoot%\Library\curl-7.77.0\usr\lib\cmake\CURL ^
  -D ICU_ROOT=%BuildRoot%\Library\icu-67.1 ^
  -D ICU_UC_LIBRARY=%BuildRoot%\Library\icu-67.1\lib64\icuuc67.lib ^
  -D ICU_I18N_LIBRARY=%BuildRoot%\Library\icu-67.1\lib64\icuin67.lib ^
  -D LIBXML2_LIBRARY=%BuildRoot%\Library\libxml2-2.9.12\usr\lib\libxml2s.lib ^
  -D LIBXML2_INCLUDE_DIR=%BuildRoot%\Library\libxml2-2.9.12\usr\include\libxml2 ^
  -D LIBXML2_DEFINITIONS="/DLIBXML_STATIC" ^
  -D ZLIB_LIBRARY=%BuildRoot%\Library\zlib-1.2.11\usr\lib\zlibstatic.lib ^
  -D ZLIB_INCLUDE_DIR=%BuildRoot%\Library\zlib-1.2.11\usr\include ^
  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D XCTest_DIR=%BuildRoot%\5\cmake\modules ^

  -D ENABLE_TESTING=YES ^

  -G Ninja ^
  -S %SourceRoot%\swift-corelibs-foundation || (exit /b)
cmake --build %BuildRoot%\4 || (exit /b)

:: Test Foundation
set CTEST_OUTPUT_ON_FAILURE=1
cmake --build %BuildRoot%\4 --target test || (exit /b)

:: Rebuild XCTest (w/ testing)
cmake ^
  -B %BuildRoot%\5 ^

  -D CMAKE_BUILD_TYPE=%CMAKE_BUILD_TYPE% ^
  -D CMAKE_C_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=%BuildRoot%/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=%BuildRoot%/1/bin/swiftc.exe ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_INSTALL_PREFIX=%PlatformRoot%\Developer\Library\XCTest-development\usr ^

  -D dispatch_DIR=%BuildRoot%\3\cmake\modules ^
  -D Foundation_DIR=%BuildRoot%\4\cmake\modules ^

  -D ENABLE_TESTING=YES ^
  -D XCTEST_PATH_TO_LIBDISPATCH_BUILD=%BuildRoot%\3 ^
  -D XCTEST_PATH_TO_LIBDISPATCH_SOURCE=%SourceRoot%\swift-corelibs-libdispatch ^
  -D XCTEST_PATH_TO_FOUNDATION_BUILD=%BuildRoot%\4 ^

  -G Ninja ^
  -S %SourceRoot%\swift-corelibs-xctest || (exit /b)
cmake --build %BuildRoot%\5 || (exit /b)

:: Test XCTest
cmake --build %BuildRoot%\5 --target check-xctest || (exit /b)

:: Clean up the module cache
rd /s /q %LocalAppData%\clang\ModuleCache

goto :end
endlocal

:CloneRepositories
setlocal enableextensions enabledelayedexpansion

if defined SKIP_UPDATE_CHECKOUT goto :eof

if defined REPO_SCHEME set "args=--scheme %REPO_SCHEME%"

:: Always enable symbolic links
git config --global core.symlink true

:: Ensure that we have the files in the original line endings, the swift tests
:: depend on this being the case.
git -C "%SourceRoot%\swift" config --local core.autocrlf input
git -C "%SourceRoot%\swift" checkout-index --force --all

set "args=%args% --skip-repository swift"
set "args=%args% --skip-repository ninja"
set "args=%args% --skip-repository icu"
set "args=%args% --skip-repository swift-integration-tests"
set "args=%args% --skip-repository swift-stress-tester"
set "args=%args% --skip-repository swift-xcode-playground-support"

call "%SourceRoot%\swift\utils\update-checkout.cmd" %args% --clone --skip-tags --skip-history --github-comment "%ghprbCommentBody%"

goto :eof
endlocal

:CloneDependencies
setlocal enableextensions enabledelayedexpansion

:: Always enable symbolic links
git config --global core.symlink true

:: FIXME(compnerd) avoid the fresh clone
rd /s /q zlib libxml2 sqlite icu curl

git clone --quiet --no-tags --depth 1 --branch v1.2.11 https://github.com/madler/zlib
git clone --quiet --no-tags --depth 1 --branch v2.9.12 https://github.com/gnome/libxml2
git clone --quiet --no-tags --depth 1 --branch version-3.36.0 https://github.com/sqlite/sqlite
git clone --quiet --no-tags --depth 1 --branch maint/maint-67 https://github.com/unicode-org/icu
git clone --quiet --no-tags --depth 1 --branch curl-7_77_0 https://github.com/curl/curl

goto :eof
endlocal

:end
