# Use anyAppleOS availability (UseAnyAppleOSAvailability)

An opt-in warning that suggests replacing a group of per-platform
`@available` specifications with a single `anyAppleOS` specification when the
platforms share the same introduced version.

## Overview

`anyAppleOS` is a meta-platform that covers all Apple platforms using unified
versioning starting at 26.0. When a declaration is available on at least
`macOS`, `iOS`, `tvOS`, and `watchOS` at the same introduced version, a single
`anyAppleOS` specification expresses the same intent more concisely.

This warning is opt-in and disabled by default. Enable it with:

```
-Wwarning UseAnyAppleOSAvailability
```

or promote it to an error with `-Werror UseAnyAppleOSAvailability`.

## Examples

Short-form grouped availability:

```swift
@available(macOS 26, iOS 26, tvOS 26, watchOS 26, *)
func foo() {} // warning: use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version

// Suggested replacement
@available(anyAppleOS 26, *)
func foo() {}
```
