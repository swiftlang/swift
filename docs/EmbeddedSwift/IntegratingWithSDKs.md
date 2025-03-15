# Embedded Swift -- Integrating with embedded SDKs

**⚠️ Embedded Swift is experimental. This document might be out of date with latest development.**

**‼️ Use the latest downloadable 'Trunk Development' snapshot from swift.org to use Embedded Swift. Public releases of Swift do not yet support Embedded Swift.**

For an introduction and motivation into Embedded Swift, please see "[A Vision for Embedded Swift](https://github.com/swiftlang/swift-evolution/blob/main/visions/embedded-swift.md)", a Swift Evolution document highlighting the main goals and approaches.

The following document sketches how to integrate Swift code into some popular embedded platforms' SDKs and build systems.

## Integrating with Raspberry Pi Pico / Pico W / Pico 2 build system:

Development for [Raspberry Pi Pico and Pico W](https://www.raspberrypi.com/products/raspberry-pi-pico/) normally uses the [Pico SDK](https://github.com/raspberrypi/pico-sdk) and the vendor provides several [sample projects in the pico-examples repository](https://github.com/raspberrypi/pico-examples). The SDK and sample project setup is described in:

- https://www.raspberrypi.com/documentation/microcontrollers/c_sdk.html#sdk-setup
- https://datasheets.raspberrypi.com/pico/getting-started-with-pico.pdf

Before trying to use Swift with the Pico SDK, make sure your environment works and can build the provided C/C++ sample projects.

### CMake setup with a bridging header

The Pico SDK is using CMake as its build system, and so the simplest way to integrate with it is to also use CMake to build a Swift firmware application on top of the SDK and the libraries from it. The following describes an example set up of that on a "blinky" example (code that just blinks the built-in LED).

Let's create a directory with a Swift source file, a bridging header, and a CMake definition file:

```
./SwiftPicoBlinky/Main.swift
./SwiftPicoBlinky/BridgingHeader.h
./SwiftPicoBlinky/CMakeLists.txt
```

In `Main.swift`, let's add basic logic to initialize the GPIO port for the Pico's built-in LED, and then turn it on and off in a loop:

```swift
@main
struct Main {
  static func main() {
    let led = UInt32(PICO_DEFAULT_LED_PIN)
    gpio_init(led)
    gpio_set_dir(led, /*out*/true)
    while true {
      gpio_put(led, true)
      sleep_ms(250)
      gpio_put(led, false)
      sleep_ms(250)
    }
  }
}
```

Notice that we're using functions and variables defined in C in the Pico SDK. For that to be possible, the Swift compiler needs to have access to the C header files that define these functions and variables. The cleanest option would be to define a modulemap, but for simplicity let's just use a bridging header to make declarations visible in Swift without a module. `BridgingHeader.h` should contain:

```c
#pragma once

#include "pico/stdlib.h"
```

Finally, we need to define the application's build rules in CMake that will be using CMake logic from the Pico SDK. The following content of `CMakeLists.txt` shows how to *manually call swiftc, the Swift compiler* instead of using the recently added CMake native support for Swift, so that we can see the full Swift compilation command.

We'll make sure to dynamically set the Swift compiler's target architecture based on the pico board used. This is to support both RP2040 and RP2350 in ARM mode, and RP2350's RISC-V mode.

We'll also ensure to recursively gather all Pico SDK-related compiler definitions in order to append them to our `swiftc` command.

```cmake
cmake_minimum_required(VERSION 3.13)
include($ENV{PICO_SDK_PATH}/external/pico_sdk_import.cmake)

project(swift-blinky)
pico_sdk_init()

if(APPLE)
execute_process(COMMAND xcrun -f swiftc OUTPUT_VARIABLE SWIFTC OUTPUT_STRIP_TRAILING_WHITESPACE)
else()
execute_process(COMMAND which swiftc OUTPUT_VARIABLE SWIFTC OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()

# Dinamically set the architecture based on the Pico board used.
set(SWIFT_TARGET "armv6m-none-none-eabi")

if(PICO_PLATFORM STREQUAL "rp2350-arm-s")
    message(STATUS "PICO_PLATFORM is set to rp2350-arm-s, using armv7em")
    set(SWIFT_TARGET "armv7em-none-none-eabi")
    list(APPEND CLANG_ARCH_ABI_FLAGS "-Xcc" "-mfloat-abi=soft")
elseif(PICO_PLATFORM STREQUAL "rp2040")
    message(STATUS "PICO_PLATFORM is set to RP2040, using armv6m")
    list(APPEND CLANG_ARCH_ABI_FLAGS "-Xcc" "-mfloat-abi=soft")
elseif(PICO_PLATFORM STREQUAL "rp2350-riscv")
    message(STATUS "PICO_PLATFORM is set to rp2350-riscv, using riscv32.")
    set(SWIFT_TARGET "riscv32-none-none-eabi")
    list(APPEND CLANG_ARCH_ABI_FLAGS "-Xcc" "-march=rv32imac_zicsr_zifencei_zba_zbb_zbs_zbkb" "-Xcc" "-mabi=ilp32")
endif()

add_executable(swift-blinky)

# You may need to add additional libraries here, if you're using the Pico W.
target_link_libraries(swift-blinky
    pico_stdlib hardware_uart hardware_gpio
)

# Gather compile definitions from all dependencies

set_property(GLOBAL PROPERTY visited_targets "")
set_property(GLOBAL PROPERTY compilerdefs_list "")

function(gather_compile_definitions_recursive target)
    # Get the current value of visited_targets
    get_property(visited_targets GLOBAL PROPERTY visited_targets)
    
    # make sure we don't visit the same target twice
    # and that we don't visit the special generator expressions
    if (${target} MATCHES "\\$<" OR ${target} MATCHES "::@" OR ${target} IN_LIST visited_targets)
        return()
    endif()

    # Append the target to visited_targets
    list(APPEND visited_targets ${target})
    set_property(GLOBAL PROPERTY visited_targets "${visited_targets}")

    # Get the current value of compilerdefs_list
    get_property(compilerdefs_list GLOBAL PROPERTY compilerdefs_list)

    get_target_property(target_definitions ${target} INTERFACE_COMPILE_DEFINITIONS)
    if (target_definitions)
        # Append the target definitions to compilerdefs_list
        list(APPEND compilerdefs_list ${target_definitions})
        set_property(GLOBAL PROPERTY compilerdefs_list "${compilerdefs_list}")
    endif()

    get_target_property(target_linked_libs ${target} INTERFACE_LINK_LIBRARIES)
    if (target_linked_libs)
        foreach(linked_target ${target_linked_libs})
            # Recursively gather compile definitions from dependencies
            gather_compile_definitions_recursive(${linked_target})
        endforeach()
    endif()
endfunction()

gather_compile_definitions_recursive(swift-blinky)
get_property(COMPILE_DEFINITIONS GLOBAL PROPERTY compilerdefs_list)

# Parse compiler definitions into a format that swiftc can understand
list(REMOVE_DUPLICATES COMPILE_DEFINITIONS)
list(PREPEND COMPILE_DEFINITIONS "")
string(REPLACE "$<TARGET_PROPERTY:PICO_TARGET_BINARY_TYPE>" "$<TARGET_PROPERTY:swift-blinky,PICO_TARGET_BINARY_TYPE>" COMPILE_DEFINITIONS "${COMPILE_DEFINITIONS}")
string(REPLACE ";" ";-Xcc;-D" COMPILE_DEFINITIONS "${COMPILE_DEFINITIONS}")

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/_swiftcode.o
    COMMAND
        ${SWIFTC}
        -target ${SWIFT_TARGET} -Xcc -fshort-enums
        ${COMPILE_DEFINITIONS}
        ${CLANG_ARCH_ABI_FLAGS}
        -Xfrontend -function-sections -enable-experimental-feature Embedded -wmo -parse-as-library
        $$\( echo '$<TARGET_PROPERTY:swift-blinky,INCLUDE_DIRECTORIES>' | tr '\;' '\\n' | sed -e 's/\\\(.*\\\)/-Xcc -I\\1/g' \)
        $$\( echo '${CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES}'             | tr ' '  '\\n' | sed -e 's/\\\(.*\\\)/-Xcc -I\\1/g' \)
        -import-bridging-header ${CMAKE_CURRENT_LIST_DIR}/BridgingHeader.h
        ${CMAKE_CURRENT_LIST_DIR}/Main.swift
        -c -o ${CMAKE_CURRENT_BINARY_DIR}/_swiftcode.o
    DEPENDS
        ${CMAKE_CURRENT_LIST_DIR}/BridgingHeader.h
        ${CMAKE_CURRENT_LIST_DIR}/Main.swift
)
add_custom_target(swift-blinky-swiftcode DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/_swiftcode.o)


target_link_libraries(swift-blinky
    ${CMAKE_CURRENT_BINARY_DIR}/_swiftcode.o
)
add_dependencies(swift-blinky swift-blinky-swiftcode)
pico_add_extra_outputs(swift-blinky)
```

With these three files, we can now configure and build a Swift firmware for the Pico:

```bash
$ export TOOLCHAINS=org.swift.59202401301a # any toolchain that supports embedded
$ export PICO_BOARD=pico # or any other RP-based board (including pico2)
$ export PICO_PLATFORM=<platform> # optional; possible values: rp2040, rp2350-arm-s or rp2350-riscv 
$ export PICO_SDK_PATH=<path_to_pico_sdk>
$ export PICO_TOOLCHAIN_PATH=<path_to_toolchain> # ARM / RISC-V toolchain
$ ls -al
-rw-r--r--   1 kuba  staff    39B Feb  2 22:08 BridgingHeader.h
-rw-r--r--   1 kuba  staff   1.3K Feb  2 22:08 CMakeLists.txt
-rw-r--r--   1 kuba  staff   262B Feb  2 22:08 Main.swift
$ mkdir build
$ cd build
$ cmake -S ../ -B . -G Ninja
$ ninja -v
```

This should produce several build artifacts in the `build/` subdirectory, include `swift-blinky.uf2`, which can be directly uploaded to the Pico by copying it into the fake Mass Storage Volume that the device presents when plugged over USB in BOOTSEL mode.
