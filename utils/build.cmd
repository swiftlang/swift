:: Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>

:: Don't escape variables into the calling shell
setlocal

set SourceCache=S:\SourceCache
set BinaryCache=S:\b
set InstallRoot=S:\Library
set ToolchainInstallRoot=%InstallRoot%\Developer\Toolchains\unknown-Asserts-development.xctoolchain
set PlatformInstallRoot=%InstallRoot%\Developer\Platforms\Windows.platform
set SDKInstallRoot=%PlatformInstallRoot%\Developer\SDKs\Windows.sdk

set vswhere=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe
FOR /F "delims=" %%r IN ('^""%vswhere%" -nologo -latest -products "*" -all -prerelease -property installationPath^"') DO set VsDevCmd=%%r\Common7\Tools\VsDevCmd.bat

setlocal

call "%VsDevCmd%" -no_logo -host_arch=amd64 -arch=amd64

:: toolchain
cmake                                                                           ^
  -B %BinaryCache%\1                                                            ^
  -C %SourceCache%\swift\cmake\caches\Windows-x86_64.cmake                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D CMAKE_MT=mt                                                                ^
  -D LLVM_ENABLE_PDB=YES                                                        ^
  -D LLVM_EXTERNAL_CMARK_SOURCE_DIR=%SourceCache%\cmark                         ^
  -D LLVM_EXTERNAL_SWIFT_SOURCE_DIR=%SourceCache%\swift                         ^
  -D SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY=YES                                  ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING=YES                   ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED=YES                                  ^
  -D SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING=YES                            ^
  -D SWIFT_PATH_TO_LIBDISPATCH_SOURCE=%SourceCache%\swift-corelibs-libdispatch  ^
  -D EXPERIMENTAL_STRING_PROCESSING_SOURCE_DIR=%SourceCache%\swift-experimental-string-processing ^
  -G Ninja                                                                      ^
  -S %SourceCache%\llvm-project\llvm || (exit /b)
cmake --build %BinaryCache%\1 || (exit /b)
cmake --build %BinaryCache%\1 --target install-distribution || (exit /b)

:: Restructure Internal Modules
FOR %%M IN (_InternalSwiftScan, _InternalSwiftSyntaxParser) DO (
  rd /s /q "%ToolchainInstallRoot%\usr\include\%%M"
  move /Y %ToolchainInstallRoot%\usr\lib\swift\%%M %ToolchainInstallRoot%\usr\include
  move %ToolchainInstallRoot%\usr\lib\swift\windows\%%M.lib %ToolchainInstallRoot%\usr\lib
)

:: Windows x64 Build

:: zlib
cmake                                                                           ^
  -B %BinaryCache%\zlib-1.2.11.x64                                              ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\zlib-1.2.11\usr                         ^
  -D SKIP_INSTALL_FILES=YES                                                     ^
  -G Ninja                                                                      ^
  -S %SourceCache%\zlib || (exit /b)
cmake --build %BinaryCache%\zlib-1.2.11.x64 || (exit /b)
cmake --build %BinaryCache%\zlib-1.2.11.x64 --target install || (exit /b)

:: libxml2
cmake                                                                           ^
  -B %BinaryCache%\libxml2-2.9.12.x64                                           ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\libxml2-2.9.12\usr                      ^
  -D LIBXML2_WITH_ICONV=NO                                                      ^
  -D LIBXML2_WITH_ICU=NO                                                        ^
  -D LIBXML2_WITH_LZMA=NO                                                       ^
  -D LIBXML2_WITH_PYTHON=NO                                                     ^
  -D LIBXML2_WITH_TESTS=NO                                                      ^
  -D LIBXML2_WITH_THREADS=YES                                                   ^
  -D LIBXML2_WITH_ZLIB=NO                                                       ^
  -G Ninja                                                                      ^
  -S %SourceCache%\libxml2 || (exit /b)
cmake --build %BinaryCache%\libxml2-2.9.12.x64 || (exit /b)
cmake --build %BinaryCache%\libxml2-2.9.12.x64 --target install || (exit /b)

:: curl
cmake                                                                           ^
  -B %BinaryCache%\curl-7.77.0.x64                                              ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\curl-7.77.0\usr                         ^
  -D BUILD_CURL_EXE=NO                                                          ^
  -D CMAKE_USE_OPENSSL=NO                                                       ^
  -D CURL_CA_PATH=none                                                          ^
  -D CMAKE_USE_SCHANNEL=YES                                                     ^
  -D CMAKE_USE_LIBSSH2=NO                                                       ^
  -D HAVE_POLL_FINE=NO                                                          ^
  -D CURL_DISABLE_LDAP=YES                                                      ^
  -D CURL_DISABLE_LDAPS=YES                                                     ^
  -D CURL_DISABLE_TELNET=YES                                                    ^
  -D CURL_DISABLE_DICT=YES                                                      ^
  -D CURL_DISABLE_FILE=YES                                                      ^
  -D CURL_DISABLE_TFTP=YES                                                      ^
  -D CURL_DISABLE_RTSP=YES                                                      ^
  -D CURL_DISABLE_PROXY=YES                                                     ^
  -D CURL_DISABLE_POP3=YES                                                      ^
  -D CURL_DISABLE_IMAP=YES                                                      ^
  -D CURL_DISABLE_SMTP=YES                                                      ^
  -D CURL_DISABLE_GOPHER=YES                                                    ^
  -D CURL_ZLIB=YES                                                              ^
  -D ENABLE_UNIX_SOCKETS=NO                                                     ^
  -D ENABLE_THREADED_RESOLVER=NO                                                ^
  -D ZLIB_ROOT=%InstallRoot%\zlib-1.2.11\usr                                    ^
  -D ZLIB_LIBRARY=%InstallRoot%\zlib-1.2.11\usr\lib\zlibstatic.lib              ^
  -G Ninja                                                                      ^
  -S %SourceCache%\curl || (exit /b)
cmake --build %BinaryCache%\curl-7.77.0.x64 || (exit /b)
cmake --build %BinaryCache%\curl-7.77.0.x64 --target install || (exit /b)

:: icu
IF NOT EXIST %SourceCache%\icu\icu4c\CMakeLists.txt copy %SourceCache%\swift-installer-scripts\shared\ICU\CMakeLists.txt %SourceCache%\icu\icu4c\CMakeLists.txt
cmake                                                                           ^
  -B %BinaryCache%\icu-69.1.x64                                                 ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\icu-69.1\usr                            ^
  -D BUILD_TOOLS=YES                                                            ^
  -G Ninja                                                                      ^
  -S %SourceCache%\icu\icu4c || (exit /b)
cmake --build %BinaryCache%\icu-69.1.x64 || (exit /b)
cmake --build %BinaryCache%\icu-69.1.x64 --target install || (exit /b)

:: sqlite
md S:\var\cache
IF NOT EXIST S:\var\cache\sqlite-amalgamation-3360000.zip curl -sL https://sqlite.org/2021/sqlite-amalgamation-3360000.zip -o S:\var\cache\sqlite-amalgamation-3360000.zip
IF NOT EXIST %SourceCache%\sqlite-3.36.0  (
  md %SourceCache%\sqlite-3.36.0
  "%ProgramFiles%\Git\usr\bin\unzip.exe" -j -o S:\var\cache\sqlite-amalgamation-3360000.zip -d %SourceCache%\sqlite-3.36.0
  copy /Y %SourceCache%\swift-build\cmake\SQLite\CMakeLists.txt %SourceCache%\sqlite-3.36.0\
)

cmake                                                                           ^
  -B %BinaryCache%\sqlite-3.36.0.x64                                            ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\sqlite-3.36.0\usr                       ^
  -D CMAKE_MT=mt                                                                ^
  -G Ninja                                                                      ^
  -S %SourceCache%\sqlite-3.36.0 || (exit /b)
cmake --build %BinaryCache%\sqlite-3.36.0.x64 || (exit /b)
cmake --build %BinaryCache%\sqlite-3.36.0.x64 --target install || (exit /b)

:: LLVM
cmake                                                                           ^
  -B %BinaryCache%\100                                                          ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D LLVM_HOST_TRIPLE=x86_64-unknown-windows-msvc                               ^
  -G Ninja                                                                      ^
  -S %SourceCache%\llvm-project\llvm || (exit /b)

:: Swift Runtime
cmake                                                                           ^
  -B %BinaryCache%\101                                                          ^
  -C %SourceCache%\swift\cmake\caches\Runtime-Windows-x86_64.cmake              ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr                                  ^
  -D CMAKE_MT=mt                                                                ^
  -D LLVM_DIR=%BinaryCache%\100\lib\cmake\llvm                                  ^
  -D SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY=YES                                  ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING=YES                   ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED=YES                                  ^
  -D SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING=YES                            ^
  -D SWIFT_NATIVE_SWIFT_TOOLS_PATH=%BinaryCache%\1\bin                          ^
  -D SWIFT_PATH_TO_LIBDISPATCH_SOURCE=%SourceCache%\swift-corelibs-libdispatch  ^
  -D EXPERIMENTAL_STRING_PROCESSING_SOURCE_DIR=%SourceCache%\swift-experimental-string-processing ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift || (exit /b)
cmake --build %BinaryCache%\101 || (exit /b)
cmake --build %BinaryCache%\101 --target install || (exit /b)

:: Restructure Runtime
md %InstallRoot%\swift-development\usr\bin\x64
move /Y %SDKInstallRoot%\usr\bin\*.dll %InstallRoot%\swift-development\usr\bin\x64\

:: SDKSettings.plist
"%ProgramFiles(x86)%\Microsoft Visual Studio\Shared\Python39_64\python.exe" -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'DEFAULT_USE_RUNTIME': 'MD' } }), encoding='utf-8'))" > %SDKInstallRoot%\SDKSettings.plist

:: swift-corelibs-libdispatch
cmake                                                                           ^
  -B %BinaryCache%\102                                                          ^
  -D BUILD_TESTING=NO                                                           ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe                                 ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr                                  ^
  -D CMAKE_MT=mt                                                                ^
  -D ENABLE_SWIFT=YES                                                           ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-corelibs-libdispatch || (exit /b)
cmake --build %BinaryCache%\102 || (exit /b)
cmake --build %BinaryCache%\102 --target install || (exit /b)

:: Restructure Runtime
move /Y %SDKInstallRoot%\usr\bin\*.dll %InstallRoot%\swift-development\usr\bin\x64\

:: Restructure BlocksRuntime, dispatch headers
FOR %%M IN (Block, dispatch, os) DO (
  rd /s /q %SDKInstallRoot%\usr\include\%%M
  move /Y %SDKInstallRoot%\usr\lib\swift\%%M %SDKInstallRoot%\usr\include\
)

:: Restructure Import Libraries
FOR %%M IN (BlocksRuntime, dispatch, swiftDispatch) DO (
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\%%M.lib %SDKInstallRoot%\usr\lib\swift\windows\x86_64
)

:: Restructure Module
md %SDKInstallRoot%\usr\lib\swift\windows\Dispatch.swiftmodule
move /Y %SDKInstallRoot%\usr\lib\swift\windows\x86_64\Dispatch.swiftmodule %SDKInstallRoot%\usr\lib\swift\windows\Dispatch.swiftmodule\x86_64-unknown-windows-msvc.swiftmodule
move /Y %SDKInstallRoot%\usr\lib\swift\windows\x86_64\Dispatch.swiftdoc %SDKInstallRoot%\usr\lib\swift\windows\Dispatch.swiftmodule\x86_64-unknown-windows-msvc.swiftdoc

:: swift-corelibs-foundation
cmake                                                                           ^
  -B %BinaryCache%\103                                                          ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr                                  ^
  -D CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDLL="/MD"      ^
  -D CMAKE_MT=mt                                                                ^
  -D CURL_DIR=%InstallRoot%\curl-7.77.0\usr\lib\cmake\CURL                      ^
  -D ICU_I18N_LIBRARY_RELEASE=%InstallRoot%\icu-69.1\usr\lib\icuin69.lib        ^
  -D ICU_ROOT=%InstallRoot%\icu-69.1\usr                                        ^
  -D ICU_UC_LIBRARY_RELEASE=%InstallRoot%\icu-69.1\usr\lib\icuuc69.lib          ^
  -D LIBXML2_LIBRARY=%InstallRoot%\libxml2-2.9.12\usr\lib\libxml2s.lib          ^
  -D LIBXML2_INCLUDE_DIR=%InstallRoot%\libxml2-2.9.12\usr\include\libxml2       ^
  -D LIBXML2_DEFINITIONS="/DLIBXML_STATIC"                                      ^
  -D ZLIB_LIBRARY=%InstallRoot%\zlib-1.2.11\usr\lib\zlibstatic.lib              ^
  -D ZLIB_INCLUDE_DIR=%InstallRoot%\zlib-1.2.11\usr\include                     ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D ENABLE_TESTING=NO                                                          ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-corelibs-foundation || (exit /b)
cmake --build %BinaryCache%\103 || (exit /b)
cmake --build %BinaryCache%\103 --target install || (exit /b)

:: Restructure Runtime
move /Y %SDKInstallRoot%\usr\bin\*.dll %InstallRoot%\swift-development\usr\bin\x64\
move /Y %SDKInstallRoot%\usr\bin\*.exe %InstallRoot%\swift-development\usr\bin\x64\

:: Remove CoreFoundation Headers
FOR %%M IN (CoreFoundation, CFXMLInterface, CFURLSessionInterface) DO (
  rd /s /q %SDKInstallRoot%\usr\lib\swift\%%M
)

:: Restructure Import Libraries, Modules
FOR %%M IN (Foundation, FoundationNetworking, FoundationXML) DO (
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\%%M.lib %SDKInstallRoot%\usr\lib\swift\windows\x86_64

  md %SDKInstallRoot%\usr\lib\swift\windows\%%M.swiftmodule
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\x86_64\%%M.swiftmodule %SDKInstallRoot%\usr\lib\swift\windows\%%M.swiftmodule\x86_64-unknown-windows-msvc.swiftmodule
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\x86_64\%%M.swiftdoc %SDKInstallRoot%\usr\lib\swift\windows\%%M.swiftmodule\x86_64-unknown-windows-msvc.swiftdoc
)

:: swift-corelibs-xctest
cmake                                                                           ^
  -B %BinaryCache%\104                                                          ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%PlatformInstallRoot%\Developer\Library\XCTest-development\usr ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\103\cmake\modules                             ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-corelibs-xctest || (exit /b)
cmake --build %BinaryCache%\104 || (exit /b)
cmake --build %BinaryCache%\104 --target install || (exit /b)

:: Restructure Runtime
rd /s /q %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\bin64
move %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\bin %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\bin64

:: Restructure Import Libraries
md %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\x86_64\
move %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\XCTest.lib %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\x86_64\XCTest.lib

:: Restructure Module
md %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\XCTest.swiftmodule
move /Y %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\x86_64\XCTest.swiftdoc %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\XCTest.swiftmodule\x86_64-unknown-windows-msvc.swiftdoc
move /Y %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\x86_64\XCTest.swiftmodule %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\XCTest.swiftmodule\x86_64-unknown-windows-msvc.swiftmodule

:: Info.plist
"%ProgramFiles(x86)%\Microsoft Visual Studio\Shared\Python39_64\python.exe" -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'XCTEST_VERSION': 'development' } }), encoding='utf-8'))" > %PlatformInstallRoot%\Info.plist

:: swift-system
cmake                                                                           ^
  -B %BinaryCache%\2                                                            ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D CMAKE_MT=mt                                                                ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-system || (exit /b)
cmake --build %BinaryCache%\2 || (exit /b)
cmake --build %BinaryCache%\2 --target install || (exit /b)

:: tools-support-core
cmake                                                                           ^
  -B %BinaryCache%\3                                                            ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D CMAKE_MT=mt                                                                ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\103\cmake\modules                             ^
  -D SwiftSystem_DIR=%BinaryCache%\2\cmake\modules                              ^
  -D SQLite3_INCLUDE_DIR=%InstallRoot%\sqlite-3.36.0\usr\include                ^
  -D SQLite3_LIBRARY=%InstallRoot%\sqlite-3.36.0\usr\lib\SQLite3.lib            ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-tools-support-core || (exit /b)
cmake --build %BinaryCache%\3 || (exit /b)
cmake --build %BinaryCache%\3 --target install || (exit /b)

:: llbuild
cmake                                                                           ^
  -B %BinaryCache%\4                                                            ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe                                 ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_CXX_FLAGS="-Xclang -fno-split-cold-code"                             ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D CMAKE_MT=mt                                                                ^
  -D LLBUILD_SUPPORT_BINDINGS=Swift                                             ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\103\cmake\modules                             ^
  -D SQLite3_INCLUDE_DIR=%InstallRoot%\sqlite-3.36.0\usr\include                ^
  -D SQLite3_LIBRARY=%InstallRoot%\sqlite-3.36.0\usr\lib\SQLite3.lib            ^
  -G Ninja                                                                      ^
  -S %SourceCache%\llbuild || (exit /b)
cmake --build %BinaryCache%\4 || (exit /b)
cmake --build %BinaryCache%\4 --target install || (exit /b)

:: Yams
cmake                                                                           ^
  -B %BinaryCache%\5                                                            ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\103\cmake\modules                             ^
  -D XCTest_DIR=%BinaryCache%\104\cmake\modules                                 ^
  -G Ninja                                                                      ^
  -S %SourceCache%\Yams || (exit /b)
cmake --build %BinaryCache%\5 || (exit /b)
cmake --build %BinaryCache%\5 --target install || (exit /b)

:: swift-argument-parser
cmake                                                                           ^
  -B %BinaryCache%\6                                                            ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D BUILD_TESTING=NO                                                           ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\103\cmake\modules                             ^
  -D XCTest_DIR=%BinaryCache%\104\cmake\modules                                 ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-argument-parser || (exit /b)
cmake --build %BinaryCache%\6 || (exit /b)
cmake --build %BinaryCache%\6 --target install || (exit /b)

:: swift-driver
cmake                                                                           ^
  -B %BinaryCache%\7                                                            ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\103\cmake\modules                             ^
  -D SwiftSystem_DIR=%BinaryCache%\2\cmake\modules                              ^
  -D TSC_DIR=%BinaryCache%\3\cmake\modules                                      ^
  -D LLBuild_DIR=%BinaryCache%\4\cmake\modules                                  ^
  -D Yams_DIR=%BinaryCache%\5\cmake\modules                                     ^
  -D ArgumentParser_DIR=%BinaryCache%\6\cmake\modules                           ^
  -D SQLite3_INCLUDE_DIR=%InstallRoot%\sqlite-3.36.0\usr\include                ^
  -D SQLite3_LIBRARY=%InstallRoot%\sqlite-3.36.0\usr\lib\SQLite3.lib            ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-driver || (exit /b)
cmake --build %BinaryCache%\7 || (exit /b)
cmake --build %BinaryCache%\7 --target install || (exit /b)

:: Switch to swift-driver
copy /Y %BinaryCache%\10\bin\swift-driver.exe %ToolchainInstallRoot%\usr\bin\swift.exe
copy /Y %BinaryCache%\10\bin\swift-driver.exe %ToolchainInstallRoot%\usr\bin\swiftc.exe

:: swift-crypto
cmake                                                                           ^
  -B %BinaryCache%\8                                                            ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\103\cmake\modules                             ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-crypto || (exit /b)
cmake --build %BinaryCache%\8 || (exit /b)
cmake --build %BinaryCache%\8 --target install || (exit /b)

:: swift-collections
cmake                                                                           ^
  -B %BinaryCache%\9                                                            ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-collections || (exit /b)
cmake --build %BinaryCache%\9 || (exit /b)
cmake --build %BinaryCache%\9 --target install || (exit /b)

:: swift-package-manager
cmake                                                                           ^
  -B %BinaryCache%\10                                                           ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_Swift_FLAGS="-DCRYPTO_v2"                                            ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D CMAKE_MT=mt                                                                ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\103\cmake\modules                             ^
  -D SwiftSystem_DIR=%BinaryCache%\2\cmake\modules                              ^
  -D TSC_DIR=%BinaryCache%\3\cmake\modules                                      ^
  -D LLBuild_DIR=%BinaryCache%\4\cmake\modules                                  ^
  -D ArgumentParser_DIR=%BinaryCache%\6\cmake\modules                           ^
  -D SwiftDriver_DIR=%BinaryCache%\7\cmake\modules                              ^
  -D SwiftCrypto_DIR=%BinaryCache%\8\cmake\modules                              ^
  -D SwiftCollections_DIR=%BinaryCache%\9\cmake\modules                         ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-package-manager || (exit /b)
cmake --build %BinaryCache%\10 || (exit /b)
cmake --build %BinaryCache%\10 --target install || (exit /b)

:: indexstore-db
cmake                                                                           ^
  -B %BinaryCache%\11                                                           ^
  -D BUILD_SHARED_LIBS=YES                                                      ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_CXX_FLAGS="-Xclang -fno-split-cold-code"                             ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe                                 ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D CMAKE_MT=mt                                                                ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\103\cmake\modules                             ^
  -G Ninja                                                                      ^
  -S %SourceCache%\indexstore-db || (exit /b)
cmake --build %BinaryCache%\11 || (exit /b)
cmake --build %BinaryCache%\11 --target install || (exit /b)

:: sourcekit-lsp
cmake                                                                           ^
  -B %BinaryCache%\12                                                           ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_INSTALL_PREFIX=%ToolchainInstallRoot%\usr                            ^
  -D CMAKE_MT=mt                                                                ^
  -D dispatch_DIR=%BinaryCache%\102\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\103\cmake\modules                             ^
  -D SwiftSystem_DIR=%BinaryCache%\2\cmake\modules                              ^
  -D TSC_DIR=%BinaryCache%\3\cmake\modules                                      ^
  -D LLBuild_DIR=%BinaryCache%\4\cmake\modules                                  ^
  -D ArgumentParser_DIR=%BinaryCache%\6\cmake\modules                           ^
  -D SwiftCollections_DIR=%BinaryCache%\9\cmake\modules                         ^
  -D SwiftPM_DIR=%BinaryCache%\10\cmake\modules                                 ^
  -D IndexStoreDB_DIR=%BinaryCache%\11\cmake\modules                            ^
  -G Ninja                                                                      ^
  -S %SourceCache%\sourcekit-lsp || (exit /b)
cmake --build %BinaryCache%\12 || (exit /b)
cmake --build %BinaryCache%\12 --target install || (exit /b)

endlocal

:: Windows x86 Build

setlocal

call "%VsDevCmd%" -no_logo -host_arch=amd64 -arch=x86

:: zlib
cmake                                                                           ^
  -B %BinaryCache%\zlib-1.2.11.x86                                              ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\zlib-1.2.11\usr                         ^
  -D INSTALL_BIN_DIR=%InstallRoot%\zlib-1.2.11\usr\bin\x86                      ^
  -D INSTALL_LIB_DIR=%InstallRoot%\zlib-1.2.11\usr\lib\x86                      ^
  -D SKIP_INSTALL_FILES=YES                                                     ^
  -G Ninja                                                                      ^
  -S %SourceCache%\zlib || (exit /b)
cmake --build %BinaryCache%\zlib-1.2.11.x86 || (exit /b)
cmake --build %BinaryCache%\zlib-1.2.11.x86 --target install || (exit /b)

:: libxml2
cmake                                                                           ^
  -B %BinaryCache%\libxml2-2.9.12.x86                                           ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\libxml2-2.9.12\usr                      ^
  -D CMAKE_INSTALL_BINDIR=bin/x86                                               ^
  -D CMAKE_INSTALL_LIBDIR=lib/x86                                               ^
  -D LIBXML2_WITH_ICONV=NO                                                      ^
  -D LIBXML2_WITH_ICU=NO                                                        ^
  -D LIBXML2_WITH_LZMA=NO                                                       ^
  -D LIBXML2_WITH_PYTHON=NO                                                     ^
  -D LIBXML2_WITH_TESTS=NO                                                      ^
  -D LIBXML2_WITH_THREADS=YES                                                   ^
  -D LIBXML2_WITH_ZLIB=NO                                                       ^
  -G Ninja                                                                      ^
  -S %SourceCache%\libxml2 || (exit /b)
cmake --build %BinaryCache%\libxml2-2.9.12.x86 || (exit /b)
cmake --build %BinaryCache%\libxml2-2.9.12.x86 --target install || (exit /b)

:: curl
cmake                                                                           ^
  -B %BinaryCache%\curl-7.77.0.x86                                              ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\curl-7.77.0\usr                         ^
  -D CMAKE_INSTALL_BINDIR=bin/x86                                               ^
  -D CMAKE_INSTALL_LIBDIR=lib/x86                                               ^
  -D BUILD_CURL_EXE=NO                                                          ^
  -D CMAKE_USE_OPENSSL=NO                                                       ^
  -D CURL_CA_PATH=none                                                          ^
  -D CMAKE_USE_SCHANNEL=YES                                                     ^
  -D CMAKE_USE_LIBSSH2=NO                                                       ^
  -D HAVE_POLL_FINE=NO                                                          ^
  -D CURL_DISABLE_LDAP=YES                                                      ^
  -D CURL_DISABLE_LDAPS=YES                                                     ^
  -D CURL_DISABLE_TELNET=YES                                                    ^
  -D CURL_DISABLE_DICT=YES                                                      ^
  -D CURL_DISABLE_FILE=YES                                                      ^
  -D CURL_DISABLE_TFTP=YES                                                      ^
  -D CURL_DISABLE_RTSP=YES                                                      ^
  -D CURL_DISABLE_PROXY=YES                                                     ^
  -D CURL_DISABLE_POP3=YES                                                      ^
  -D CURL_DISABLE_IMAP=YES                                                      ^
  -D CURL_DISABLE_SMTP=YES                                                      ^
  -D CURL_DISABLE_GOPHER=YES                                                    ^
  -D CURL_ZLIB=YES                                                              ^
  -D ENABLE_UNIX_SOCKETS=NO                                                     ^
  -D ENABLE_THREADED_RESOLVER=NO                                                ^
  -D ZLIB_ROOT=%InstallRoot%\zlib-1.2.11\usr                                    ^
  -D ZLIB_LIBRARY=%InstallRoot%\zlib-1.2.11\usr\lib\x86\zlibstatic.lib          ^
  -G Ninja                                                                      ^
  -S %SourceCache%\curl || (exit /b)
cmake --build %BinaryCache%\curl-7.77.0.x86 || (exit /b)
cmake --build %BinaryCache%\curl-7.77.0.x86 --target install || (exit /b)

:: icu
cmake                                                                           ^
  -B %BinaryCache%\icu-69.1.x86                                                 ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D BUILD_TOOLS=YES                                                            ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\icu-69.1\usr                            ^
  -D CMAKE_INSTALL_BINDIR=bin/x86                                               ^
  -D CMAKE_INSTALL_LIBDIR=lib/x86                                               ^
  -G Ninja                                                                      ^
  -S %SourceCache%\icu\icu4c || (exit /b)
cmake --build %BinaryCache%\icu-69.1.x86 || (exit /b)
cmake --build %BinaryCache%\icu-69.1.x86 --target install || (exit /b)

:: Swift Runtime
cmake                                                                           ^
  -B %BinaryCache%\201                                                          ^
  -C %SourceCache%\swift\cmake\caches\Runtime-Windows-i686.cmake                ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_C_COMPILER_TARGET=i686-unknown-windows-msvc                          ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe                                 ^
  -D CMAKE_CXX_COMPILER_TARGET=i686-unknown-windows-msvc                        ^
  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr                                  ^
  -D CMAKE_MT=mt                                                                ^
  -D LLVM_DIR=%BinaryCache%\100\lib\cmake\llvm                                  ^
  -D SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY=YES                                  ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING=YES                   ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED=YES                                  ^
  -D SWIFT_NATIVE_SWIFT_TOOLS_PATH=%BinaryCache%\1\bin                          ^
  -D SWIFT_PATH_TO_LIBDISPATCH_SOURCE=%SourceCache%\swift-corelibs-libdispatch  ^
  -D EXPERIMENTAL_STRING_PROCESSING_SOURCE_DIR=%SourceCache%\swift-experimental-string-processing ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift || (exit /b)
cmake --build %BinaryCache%\201 || (exit /b)
cmake --build %BinaryCache%\201 --target install || (exit /b)

:: Restructure Runtime
md %InstallRoot%\swift-development\usr\bin\x86
move /Y %SDKInstallRoot%\usr\bin\*.dll %InstallRoot%\swift-development\usr\bin\x86\

:: swift-corelibs-libdispatch
cmake                                                                           ^
  -B %BinaryCache%\202                                                          ^
  -D BUILD_TESTING=NO                                                           ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_C_COMPILER_TARGET=i686-unknown-windows-msvc                          ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe                                 ^
  -D CMAKE_CXX_COMPILER_TARGET=i686-unknown-windows-msvc                        ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_Swift_COMPILER_TARGET=i686-unknown-windows-msvc                      ^
  -D CMAKE_Swift_FLAGS="-resource-dir %BinaryCache%\201\lib\swift -L %BinaryCache%\201\lib\swift\windows" ^
  -D CMAKE_SYSTEM_NAME=Windows                                                  ^
  -D CMAKE_SYSTEM_PROCESSOR=i686                                                ^
  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr                                  ^
  -D CMAKE_MT=mt                                                                ^
  -D ENABLE_SWIFT=YES                                                           ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-corelibs-libdispatch || (exit /b)
cmake --build %BinaryCache%\202 || (exit /b)
cmake --build %BinaryCache%\202 --target install || (exit /b)

:: Restructure Runtime
move /Y %SDKInstallRoot%\usr\bin\*.dll %InstallRoot%\swift-development\usr\bin\x86\

:: Restructure BlocksRuntime, dispatch headers
FOR %%M IN (Block, dispatch, os) DO (
  :: TODO(compnerd) ensure that the headers are identical and that no file
  :: system corruptions or tampering has occurred.
  rd /s /q %SDKInstallRoot%\usr\lib\swift\%%M
)

:: Restructure Import Libraries
FOR %%M IN (BlocksRuntime, dispatch, swiftDispatch) DO (
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\%%M.lib %SDKInstallRoot%\usr\lib\swift\windows\i686\
)

:: Restructure Module
move /Y %SDKInstallRoot%\usr\lib\swift\windows\i686\Dispatch.swiftmodule %SDKInstallRoot%\usr\lib\swift\windows\Dispatch.swiftmodule\i686-unknown-windows-msvc.swiftmodule
move /Y %SDKInstallRoot%\usr\lib\swift\windows\i686\Dispatch.swiftdoc %SDKInstallRoot%\usr\lib\swift\windows\Dispatch.swiftmodule\i686-unknown-windows-msvc.swiftdoc

cmake                                                                           ^
  -B %BinaryCache%\203                                                          ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_ASM_COMPILER=S:/b/1/bin/clang-cl.exe                                 ^
  -D CMAKE_ASM_FLAGS="--target=i686-unknown-windows-msvc"                       ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_C_COMPILER_TARGET=i686-unknown-windows-msvc                          ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_Swift_COMPILER_TARGET=i686-unknown-windows-msvc                      ^
  -D CMAKE_Swift_FLAGS="-resource-dir %BinaryCache%\201\lib\swift -L %BinaryCache%\201\lib\swift\windows" ^
  -D CMAKE_SYSTEM_NAME=Windows                                                  ^
  -D CMAKE_SYSTEM_PROCESSOR=i686                                                ^
  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr                                  ^
  -D CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDLL="/MD"      ^
  -D CMAKE_MT=mt                                                                ^
  -D CURL_DIR=%InstallRoot%\curl-7.77.0\usr\lib\x86\cmake\CURL                  ^
  -D ICU_DATA_LIBRARY_RELEASE=%InstallRoot%\icu-69.1\usr\lib\x86\sicudt69.lib   ^
  -D ICU_I18N_LIBRARY_RELEASE=%InstallRoot%\icu-69.1\usr\lib\x86\sicuin69.lib   ^
  -D ICU_ROOT=%InstallRoot%\icu-69.1\usr                                        ^
  -D ICU_UC_LIBRARY_RELEASE=%InstallRoot%\icu-69.1\usr\lib\x86\sicuuc69.lib     ^
  -D LIBXML2_LIBRARY=%InstallRoot%\libxml2-2.9.12\usr\lib\x86\libxml2s.lib      ^
  -D LIBXML2_INCLUDE_DIR=%InstallRoot%\libxml2-2.9.12\usr\include\libxml2       ^
  -D LIBXML2_DEFINITIONS="/DLIBXML_STATIC"                                      ^
  -D ZLIB_LIBRARY=%InstallRoot%\zlib-1.2.11\usr\lib\x86\zlibstatic.lib          ^
  -D ZLIB_INCLUDE_DIR=%InstallRoot%\zlib-1.2.11\usr\include                     ^
  -D dispatch_DIR=%BinaryCache%\202\cmake\modules                               ^
  -D ENABLE_TESTING=NO                                                          ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-corelibs-foundation || (exit /b)
cmake --build %BinaryCache%\203 || (exit /b)
cmake --build %BinaryCache%\203 --target install || (exit /b)

:: Restructure Runtime
move /Y %SDKInstallRoot%\usr\bin\*.dll %InstallRoot%\swift-development\usr\bin\x86\
move /Y %SDKInstallRoot%\usr\bin\*.exe %InstallRoot%\swift-development\usr\bin\x86\

:: Remove CoreFoundation Headers
FOR %%M IN (CoreFoundation, CFXMLInterface, CFURLSessionInterface) DO (
  rd /s /q %SDKInstallRoot%\usr\lib\swift\%%M
)

:: Restructure Import Libraries, Modules
FOR %%M IN (Foundation, FoundationNetworking, FoundationXML) DO (
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\%%M.lib %SDKInstallRoot%\usr\lib\swift\windows\i686

  md %SDKInstallRoot%\usr\lib\swift\windows\i686\%%M.swiftmodule
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\i686\%%M.swiftmodule %SDKInstallRoot%\usr\lib\swift\windows\%%M.swiftmodule\i686-unknown-windows-msvc.swiftmodule
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\i686\%%M.swiftdoc %SDKInstallRoot%\usr\lib\swift\windows\%%M.swiftmodule\i686-unknown-windows-msvc.swiftdoc
)

:: swift-corelibs-xctest
cmake                                                                           ^
  -B %BinaryCache%\204                                                          ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_Swift_COMPILER_TARGET=i686-unknown-windows-msvc                      ^
  -D CMAKE_Swift_FLAGS="-resource-dir %BinaryCache%\201\lib\swift -L %BinaryCache%\201\lib\swift\windows" ^
  -D CMAKE_SYSTEM_NAME=Windows                                                  ^
  -D CMAKE_SYSTEM_PROCESSOR=i686                                                ^
  -D CMAKE_INSTALL_PREFIX=%PlatformInstallRoot%\Developer\Library\XCTest-development\usr ^
  -D dispatch_DIR=%BinaryCache%\202\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\203\cmake\modules                             ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-corelibs-xctest || (exit /b)
cmake --build %BinaryCache%\204 || (exit /b)
cmake --build %BinaryCache%\204 --target install || (exit /b)

:: Restructure Runtime
rd /s /q %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\bin32
move %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\bin %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\bin32

:: Restructure Import Libraries
md %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\i686\
move %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\XCTest.lib %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\i686\XCTest.lib

:: Restructure Module
move /Y %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\i686\XCTest.swiftmodule %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\XCTest.swiftmodule\i686-unknown-windows-msvc.swiftmodule
move /Y %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\i686\XCTest.swiftdoc %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\XCTest.swiftmodule\i686-unknown-windows-msvc.swiftdoc

endlocal

:: Windows ARM64 Runtime

setlocal

call "%VsDevCmd%" -no_logo -host_arch=amd64 -arch=arm64

:: zlib
cmake                                                                           ^
  -B %BinaryCache%\zlib-1.2.11.arm64                                            ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\zlib-1.2.11\usr                         ^
  -D INSTALL_BIN_DIR=%InstallRoot%\zlib-1.2.11\usr\bin\arm64                    ^
  -D INSTALL_LIB_DIR=%InstallRoot%\zlib-1.2.11\usr\lib\arm64                    ^
  -D SKIP_INSTALL_FILES=YES                                                     ^
  -G Ninja                                                                      ^
  -S %SourceCache%\zlib || (exit /b)
cmake --build %BinaryCache%\zlib-1.2.11.arm64 || (exit /b)
cmake --build %BinaryCache%\zlib-1.2.11.arm64 --target install || (exit /b)

:: libxml2
cmake                                                                           ^
  -B %BinaryCache%\libxml2-2.9.12.arm64                                         ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\libxml2-2.9.12\usr                      ^
  -D CMAKE_INSTALL_BINDIR=bin/arm64                                             ^
  -D CMAKE_INSTALL_LIBDIR=lib/arm64                                             ^
  -D LIBXML2_WITH_ICONV=NO                                                      ^
  -D LIBXML2_WITH_ICU=NO                                                        ^
  -D LIBXML2_WITH_LZMA=NO                                                       ^
  -D LIBXML2_WITH_PYTHON=NO                                                     ^
  -D LIBXML2_WITH_TESTS=NO                                                      ^
  -D LIBXML2_WITH_THREADS=YES                                                   ^
  -D LIBXML2_WITH_ZLIB=NO                                                       ^
  -G Ninja                                                                      ^
  -S %SourceCache%\libxml2 || (exit /b)
cmake --build %BinaryCache%\libxml2-2.9.12.arm64 || (exit /b)
cmake --build %BinaryCache%\libxml2-2.9.12.arm64 --target install || (exit /b)

:: curl
cmake                                                                           ^
  -B %BinaryCache%\curl-7.77.0.arm64                                            ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\curl-7.77.0\usr                         ^
  -D CMAKE_INSTALL_BINDIR=bin/arm64                                             ^
  -D CMAKE_INSTALL_LIBDIR=lib/arm64                                             ^
  -D BUILD_CURL_EXE=NO                                                          ^
  -D CMAKE_USE_OPENSSL=NO                                                       ^
  -D CURL_CA_PATH=none                                                          ^
  -D CMAKE_USE_SCHANNEL=YES                                                     ^
  -D CMAKE_USE_LIBSSH2=NO                                                       ^
  -D HAVE_POLL_FINE=NO                                                          ^
  -D CURL_DISABLE_LDAP=YES                                                      ^
  -D CURL_DISABLE_LDAPS=YES                                                     ^
  -D CURL_DISABLE_TELNET=YES                                                    ^
  -D CURL_DISABLE_DICT=YES                                                      ^
  -D CURL_DISABLE_FILE=YES                                                      ^
  -D CURL_DISABLE_TFTP=YES                                                      ^
  -D CURL_DISABLE_RTSP=YES                                                      ^
  -D CURL_DISABLE_PROXY=YES                                                     ^
  -D CURL_DISABLE_POP3=YES                                                      ^
  -D CURL_DISABLE_IMAP=YES                                                      ^
  -D CURL_DISABLE_SMTP=YES                                                      ^
  -D CURL_DISABLE_GOPHER=YES                                                    ^
  -D CURL_ZLIB=YES                                                              ^
  -D ENABLE_UNIX_SOCKETS=NO                                                     ^
  -D ENABLE_THREADED_RESOLVER=NO                                                ^
  -D ZLIB_ROOT=%InstallRoot%\zlib-1.2.11\usr                                    ^
  -D ZLIB_LIBRARY=%InstallRoot%\zlib-1.2.11\usr\lib\arm64\zlibstatic.lib        ^
  -G Ninja                                                                      ^
  -S %SourceCache%\curl || (exit /b)
cmake --build %BinaryCache%\curl-7.77.0.arm64 || (exit /b)
cmake --build %BinaryCache%\curl-7.77.0.arm64 --target install || (exit /b)

:: icu
cmake                                                                           ^
  -B %BinaryCache%\icu-69.1.arm64                                               ^
  -D BUILD_SHARED_LIBS=NO                                                       ^
  -D BUILD_TOOLS=NO                                                             ^
  -D ICU_TOOLS_DIR=S:\b\icu-69.1.x64                                            ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_MT=mt                                                                ^
  -D CMAKE_INSTALL_PREFIX=%InstallRoot%\icu-69.1\usr                            ^
  -D CMAKE_INSTALL_BINDIR=bin/arm64                                             ^
  -D CMAKE_INSTALL_LIBDIR=lib/arm64                                             ^
  -G Ninja                                                                      ^
  -S %SourceCache%\icu\icu4c || (exit /b)
cmake --build %BinaryCache%\icu-69.1.arm64 || (exit /b)
cmake --build %BinaryCache%\icu-69.1.arm64 --target install || (exit /b)

:: Swift Runtime
cmake                                                                           ^
  -B %BinaryCache%\301                                                          ^
  -C %SourceCache%\swift\cmake\caches\Runtime-Windows-aarch64.cmake             ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_C_COMPILER_TARGET=aarch64-unknown-windows-msvc                       ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe                                 ^
  -D CMAKE_CXX_COMPILER_TARGET=aarch64-unknown-windows-msvc                     ^
  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr                                  ^
  -D CMAKE_MT=mt                                                                ^
  -D LLVM_DIR=%BinaryCache%\100\lib\cmake\llvm                                  ^
  -D SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY=YES                                  ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING=YES                   ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED=YES                                  ^
  -D SWIFT_NATIVE_SWIFT_TOOLS_PATH=%BinaryCache%\1\bin                          ^
  -D SWIFT_PATH_TO_LIBDISPATCH_SOURCE=%SourceCache%\swift-corelibs-libdispatch  ^
  -D EXPERIMENTAL_STRING_PROCESSING_SOURCE_DIR=%SourceCache%\swift-experimental-string-processing ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift || (exit /b)
cmake --build %BinaryCache%\301 || (exit /b)
cmake --build %BinaryCache%\301 --target install || (exit /b)

:: Restructure Runtime
md %InstallRoot%\swift-development\usr\bin\arm64
move /Y %SDKInstallRoot%\usr\bin\*.dll %InstallRoot%\swift-development\usr\bin\arm64\

:: swift-corelibs-libdispatch
cmake                                                                           ^
  -B %BinaryCache%\302                                                          ^
  -D BUILD_TESTING=NO                                                           ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_C_COMPILER_TARGET=aarch64-unknown-windows-msvc                       ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe                                 ^
  -D CMAKE_CXX_COMPILER_TARGET=aarch64-unknown-windows-msvc                     ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_Swift_COMPILER_TARGET=aarch64-unknown-windows-msvc                   ^
  -D CMAKE_Swift_FLAGS="-resource-dir %BinaryCache%\301\lib\swift -L %BinaryCache%\301\lib\swift\windows" ^
  -D CMAKE_SYSTEM_NAME=Windows                                                  ^
  -D CMAKE_SYSTEM_PROCESSOR=aarch64                                             ^
  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr                                  ^
  -D CMAKE_MT=mt                                                                ^
  -D ENABLE_SWIFT=YES                                                           ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-corelibs-libdispatch || (exit /b)
cmake --build %BinaryCache%\302 || (exit /b)
cmake --build %BinaryCache%\302 --target install || (exit /b)

:: Restructure Runtime
move /Y %SDKInstallRoot%\usr\bin\*.dll %InstallRoot%\swift-development\usr\bin\arm64\

:: Restructure BlocksRuntime, dispatch headers
FOR %%M IN (Block, dispatch, os) DO (
  :: TODO(compnerd) ensure that the headers are identical and that no file
  :: system corruptions or tampering has occurred.
  rd /s /q %SDKInstallRoot%\usr\lib\swift\%%M
)

:: Restructure Import Libraries
FOR %%M IN (BlocksRuntime, dispatch, swiftDispatch) DO (
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\%%M.lib %SDKInstallRoot%\usr\lib\swift\windows\aarch64\
)

:: Restructure Module
move /Y %SDKInstallRoot%\usr\lib\swift\windows\aarch64\Dispatch.swiftmodule %SDKInstallRoot%\usr\lib\swift\windows\Dispatch.swiftmodule\aarch64-unknown-windows-msvc.swiftmodule
move /Y %SDKInstallRoot%\usr\lib\swift\windows\aarch64\Dispatch.swiftdoc %SDKInstallRoot%\usr\lib\swift\windows\Dispatch.swiftmodule\aarch64-unknown-windows-msvc.swiftdoc

cmake                                                                           ^
  -B %BinaryCache%\303                                                          ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_ASM_COMPILER=S:/b/1/bin/clang-cl.exe                                 ^
  -D CMAKE_ASM_FLAGS="--target=aarch64-unknown-windows-msvc"                    ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe                                   ^
  -D CMAKE_C_COMPILER_TARGET=aarch64-unknown-windows-msvc                       ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_Swift_COMPILER_TARGET=aarch64-unknown-windows-msvc                   ^
  -D CMAKE_Swift_FLAGS="-resource-dir %BinaryCache%\301\lib\swift -L %BinaryCache%\301\lib\swift\windows" ^
  -D CMAKE_SYSTEM_NAME=Windows                                                  ^
  -D CMAKE_SYSTEM_PROCESSOR=aarch64                                             ^
  -D CMAKE_INSTALL_PREFIX=%SDKInstallRoot%\usr                                  ^
  -D CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDLL="/MD"      ^
  -D CMAKE_MT=mt                                                                ^
  -D CURL_DIR=%InstallRoot%\curl-7.77.0\usr\lib\arm64\cmake\CURL                ^
  -D ICU_DATA_LIBRARY_RELEASE=%InstallRoot%\icu-69.1\usr\lib\arm64\sicudt69.lib ^
  -D ICU_I18N_LIBRARY_RELEASE=%InstallRoot%\icu-69.1\usr\lib\arm64\sicuin69.lib ^
  -D ICU_ROOT=%InstallRoot%\icu-69.1\usr                                        ^
  -D ICU_UC_LIBRARY_RELEASE=%InstallRoot%\icu-69.1\usr\lib\arm64\sicuuc69.lib   ^
  -D LIBXML2_LIBRARY=%InstallRoot%\libxml2-2.9.12\usr\lib\arm64\libxml2s.lib    ^
  -D LIBXML2_INCLUDE_DIR=%InstallRoot%\libxml2-2.9.12\usr\include\libxml2       ^
  -D LIBXML2_DEFINITIONS="/DLIBXML_STATIC"                                      ^
  -D ZLIB_LIBRARY=%InstallRoot%\zlib-1.2.11\usr\lib\arm64\zlibstatic.lib        ^
  -D ZLIB_INCLUDE_DIR=%InstallRoot%\zlib-1.2.11\usr\include                     ^
  -D dispatch_DIR=%BinaryCache%\302\cmake\modules                               ^
  -D ENABLE_TESTING=NO                                                          ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-corelibs-foundation || (exit /b)
cmake --build %BinaryCache%\303 || (exit /b)
cmake --build %BinaryCache%\303 --target install || (exit /b)

:: Restructure Runtime
move /Y %SDKInstallRoot%\usr\bin\*.dll %InstallRoot%\swift-development\usr\bin\arm64\
move /Y %SDKInstallRoot%\usr\bin\*.exe %InstallRoot%\swift-development\usr\bin\arm64\

:: Remove CoreFoundation Headers
FOR %%M IN (CoreFoundation, CFXMLInterface, CFURLSessionInterface) DO (
  rd /s /q %SDKInstallRoot%\usr\lib\swift\%%M
)

:: Restructure Import Libraries, Modules
FOR %%M IN (Foundation, FoundationNetworking, FoundationXML) DO (
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\%%M.lib %SDKInstallRoot%\usr\lib\swift\windows\aarch64

  md %SDKInstallRoot%\usr\lib\swift\windows\aarch64\%%M.swiftmodule
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\aarch64\%%M.swiftmodule %SDKInstallRoot%\usr\lib\swift\windows\%%M.swiftmodule\aarch64-unknown-windows-msvc.swiftmodule
  move /Y %SDKInstallRoot%\usr\lib\swift\windows\aarch64\%%M.swiftdoc %SDKInstallRoot%\usr\lib\swift\windows\%%M.swiftmodule\aarch64-unknown-windows-msvc.swiftdoc
)

:: swift-corelibs-xctest
cmake                                                                           ^
  -B %BinaryCache%\304                                                          ^
  -D CMAKE_BUILD_TYPE=Release                                                   ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe                                 ^
  -D CMAKE_Swift_COMPILER_TARGET=aarch64-unknown-windows-msvc                   ^
  -D CMAKE_Swift_FLAGS="-resource-dir %BinaryCache%\301\lib\swift -L %BinaryCache%\301\lib\swift\windows" ^
  -D CMAKE_SYSTEM_NAME=Windows                                                  ^
  -D CMAKE_SYSTEM_PROCESSOR=aarch64                                             ^
  -D CMAKE_INSTALL_PREFIX=%PlatformInstallRoot%\Developer\Library\XCTest-development\usr ^
  -D dispatch_DIR=%BinaryCache%\302\cmake\modules                               ^
  -D Foundation_DIR=%BinaryCache%\303\cmake\modules                             ^
  -G Ninja                                                                      ^
  -S %SourceCache%\swift-corelibs-xctest || (exit /b)
cmake --build %BinaryCache%\304 || (exit /b)
cmake --build %BinaryCache%\304 --target install || (exit /b)

:: Restructure Runtime
rd /s /q %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\bin64a
move %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\bin %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\bin64a

:: Restructure Import Libraries
md %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\aarch64\
move %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\XCTest.lib %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\aarch64\XCTest.lib

:: Restructure Module
move /Y %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\aarch64\XCTest.swiftmodule %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\XCTest.swiftmodule\aarch64-unknown-windows-msvc.swiftmodule
move /Y %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\aarch64\XCTest.swiftdoc %PlatformInstallRoot%\Developer\Library\XCTest-development\usr\lib\swift\windows\XCTest.swiftmodule\aarch64-unknown-windows-msvc.swiftdoc

endlocal

endlocal
