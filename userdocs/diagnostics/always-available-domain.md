# Always enabled availability domains (AlwaysAvailableDomain)

Warnings that identify `@available` attributes and `if #available` statements that reference custom availability domains which have been permanently enabled.

## Overview

Custom availability domains (which are an experimental feature gated by `-enable-experimental-feature CustomAvailability`) may be declared as "permanently enabled" which indicates that the domain is known available at compile time and can never become unavailable in a different configuration. This state is useful for representing feature flags for features which have become permanently enabled.

Restricting a region of code to only being available in a permanently enabled availability domain will result in diagnostics that suggest the removal of the restriction. For example:

```
@available(MyFeature) // warning: '@available' has no effect because 'PermanentlyEnabled' is always available
struct MyFeatureView: View {
  // ...
}
```
