# Option obsoleted by module selectors (OptionObsoletedByModuleSelectors)

Warnings that indicate a compiler option has been obsoleted by the module selectors feature ([SE-0491](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0491-module-selectors.md)).

## Overview

The module selectors feature superseded certain compiler options related to controlling the contents of generated `.swiftinterface` files. When one of these obsoleted options is passed to the compiler, it is ignored and a warning is emitted:

```
warning: ignoring '<option>'; this option has been obsoleted by module selectors (add '-disable-module-selectors-in-module-interface' to restore original behavior) [#OptionObsoletedByModuleSelectors]
```

To silence the warning, remove the obsoleted option from your build settings. If you need the original behavior that the option provided, pass `-disable-module-selectors-in-module-interface` instead.
