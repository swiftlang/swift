# Added cross import (CrossImport)

Remarks when importing a module triggers cross import of another module.

## Overview

Enable the remarks with `-Rcross-import`. This can be used to test that the cross import is working as intended.

## Using cross imports

A module declares cross-imports via a .swiftcrossimport directory next to its .swiftmodule, containing .swiftoverlay YAML files named after the "bystander" module:

```
FooKit.swiftcrossimport/
  BarKit.swiftoverlay
```

The overlay file is YAML:
```yaml
%YAML 1.2
---
version: 1
modules:
  - name: _FooBarAdditions
```

When both `FooKit` and `BarKit` are imported, `_FooBarAdditions` is implicitly imported as well. This allows shipping integrations between modules that don't explicitly depend on each other.
