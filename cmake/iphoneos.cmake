# Minimal CMake toolchain file for iOS builds.
include(CMakeForceCompiler)

set(CMAKE_SYSTEM_NAME Darwin)
set(CMAKE_SYSTEM_VERSION 13.0) # Pretend to be OS X 10.9

execute_process(COMMAND xcrun -sdk iphoneos -toolchain XcodeDefault -find clang
                OUTPUT_VARIABLE cc_path
                OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND xcrun -sdk iphoneos -toolchain XcodeDefault -find clang++
                OUTPUT_VARIABLE cxx_path
                OUTPUT_STRIP_TRAILING_WHITESPACE)

CMAKE_FORCE_C_COMPILER("${cc_path}" Clang)
CMAKE_FORCE_CXX_COMPILER("${cxx_path}" Clang)
set(CXX_SUPPORTS_CXX11 ON FORCE)

# Compiler forcing leaves the compiler version unset, which the llvm 
# build machinery doesn't like. Pacify it.
set(LLVM_FORCE_USE_OLD_TOOLCHAIN ON)
