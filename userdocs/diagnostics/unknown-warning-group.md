# Unknown warning group (UnknownWarningGroup)

Warnings for unrecognized warning groups specified in `-Wwarning` or `-Werror`.


## Overview

```sh
swiftc -Werror non_existing_group file.swift
<unknown>:0: warning: unknown warning group: 'non_existing_group'
```

Such warnings are emitted after the behavior for all specified warning groups has been processed, which means their behavior can also be specified. For example:

```sh
swiftc -Werror UnknownWarningGroup -Werror non_existing_group file.swift
<unknown>:0: error: unknown warning group: 'non_existing_group'
```
