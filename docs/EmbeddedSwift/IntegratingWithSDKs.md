# Embedded Swift -- Integrating with embedded SDKs

**⚠️ Embedded Swift is experimental. This document might be out of date with latest development.**

**‼️ Use the latest downloadable 'Trunk Development' snapshot from swift.org to use Embedded Swift. Public releases of Swift do not yet support Embedded Swift.**

For an introduction and motivation into Embedded Swift, please see "[A Vision for Embedded Swift](https://github.com/swiftlang/swift-evolution/blob/main/visions/embedded-swift.md)", a Swift Evolution document highlighting the main goals and approaches.

The following document sketches how to integrate Swift code into some popular embedded platforms' SDKs and build systems.

## Integrating with Raspberry Pi Pico (W) build system:

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

```cmake
cmake_minimum_required(VERSION 3.13)
include($ENV{PICO_SDK_PATH}/external/pico_sdk_import.cmake)

project(swift-blinky)
pico_sdk_init()
execute_process(COMMAND xcrun -f swiftc OUTPUT_VARIABLE SWIFTC OUTPUT_STRIP_TRAILING_WHITESPACE)

add_executable(swift-blinky)
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/_swiftcode.o
    COMMAND
        ${SWIFTC}
        -target armv6m-none-none-eabi -Xcc -mfloat-abi=soft -Xcc -fshort-enums
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
    pico_stdlib hardware_uart hardware_gpio
    ${CMAKE_CURRENT_BINARY_DIR}/_swiftcode.o
)
add_dependencies(swift-blinky swift-blinky-swiftcode)
pico_add_extra_outputs(swift-blinky)
```

With these three files, we can now configure and build a Swift firmware for the Pico:

```bash
$ export TOOLCHAINS=org.swift.59202401301a
$ export PICO_BOARD=pico
$ export PICO_SDK_PATH=<path_to_pico_sdk>
$ export PICO_TOOLCHAIN_PATH=<path_to_arm_toolchain>
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
