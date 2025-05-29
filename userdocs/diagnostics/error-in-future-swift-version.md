# Language mode and tools version

Swift language mode and Swift compiler tools version are distinct concepts. One compiler version can support multiple language modes.

There are two related kinds of "Swift version" that are distinct:

* Swift tools version: the version number of the compiler itself. For example, the Swift 5.6 compiler was introduced in March 2022.
* Swift language mode version: the language mode version with which we are providing source compatibility. For example, the Swift 5 language mode is the most current language mode version supported by Swift tools version 5.10.

The Swift tools support multiple Swift language modes. All Swift tools versions between 5.0 and 5.10 support three Swift language modes: 4, 4.2, and 5. The Swift 6.0 compiler supports four Swift language modes: 4, 4.2, 5, and 6. Swift language modes are opt-in; when you use a tools version that supports a new language mode, your code will not build with the new language mode until you set that language mode in your build settings with `-swift-version X`.

A warning suffixed with `this is an error in the Swift 6 language mode` means that once you migrate to that language mode in your code, the warning will be promoted to an error. These warnings primarily come from enabling upcoming features that are enabled by default in that language mode version, but errors may also be staged in as warnings until the next language mode to maintain source compatibility when fixing compiler bugs.
