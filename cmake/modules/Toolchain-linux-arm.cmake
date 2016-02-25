message(STATUS "Using Linux-arm toolchain file")

set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR "armv7l")
set(CMAKE_LIBRARY_ARCHITECTURE "arm-linux-gnueabihf") # for Glibc/CMakeLists.txt
set(CMAKE_EXECUTABLE_FORMAT "ELF")

include(CMakeForceCompiler)

# 1) Get a sysroot, e.g. by running debootstrap, or this script: https://gist.github.com/froody/de6846f3455451f81992
# 2) Download https://launchpad.net/gcc-arm-embedded/4.9/4.9-2015-q3-update/+download/gcc-arm-none-eabi-4_9-2015q3-20150921-mac.tar.bz2
# 3) Untar gcc, and in gcc-arm-none-eabi-4_9-2015q3/bin/
# 4) Get the android NDK from http://developer.android.com/ndk/downloads/index.html
#    and copy ld.gold to the gcc bin dir in (3) from:
#    toolchains/arm-linux-androideabi-4.9/prebuilt/darwin-x86_64/arm-linux-androideabi/bin/ld.gold
# 5) Run
#    ./utils/build-script -R --cross-compile-stdlib-targets linux-armv7 --
#    --cross-compile-sysroot=<sysroot path from (1)>
#    --cross-compile-toolchain-bin=<bin directory from (3)>

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
  set(CMAKE_AR ${CMAKE_C_COMPILER_EXTERNAL_TOOLCHAIN}/arm-none-eabi-ar CACHE FILEPATH "Archiver") # https://cmake.org/Bug/view.php?id=13038
endif ()

set(COMMON_C_FLAGS "-B ${CMAKE_C_COMPILER_EXTERNAL_TOOLCHAIN}")
set(COMMON_C_FLAGS "${COMMON_C_FLAGS} -B ${CMAKE_SYSROOT}/usr/lib/gcc/arm-linux-gnueabihf/4.8")
set(COMMON_C_FLAGS "${COMMON_C_FLAGS} -fuse-ld=gold")

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${COMMON_C_FLAGS}" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${COMMON_C_FLAGS}" CACHE STRING "" FORCE)

set(CMAKE_CXX_COMPILER_VERSION 3.6)

link_directories(${CMAKE_SYSROOT}/usr/lib/arm-linux-gnueabihf/
                 ${CMAKE_SYSROOT}/usr/lib/gcc/arm-linux-gnueabihf/4.8/)
include_directories(SYSTEM
                ${CMAKE_SYSROOT}/usr/include/c++/4.8/
                ${CMAKE_SYSROOT}/usr/include/arm-linux-gnueabihf/c++/4.8/
                ${CMAKE_SYSROOT}/usr/lib/gcc/arm-linux-gnueabihf/4.8/include/)

# Used to find BSD and ICU among other things
set(CMAKE_FIND_ROOT_PATH ${CMAKE_SYSROOT})

set(CMAKE_C_COMPILER_TARGET arm-linux-gnueabihf)
set(CMAKE_CXX_COMPILER_TARGET arm-linux-gnueabihf)
CMAKE_FORCE_C_COMPILER("${CMAKE_C_COMPILER}" Clang)
CMAKE_FORCE_CXX_COMPILER("${CMAKE_CXX_COMPILER}" Clang)

