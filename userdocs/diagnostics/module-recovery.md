# Modularization errors (ModularizationIssue)

Emits errors when loading incorrectly modularized modules.

## Overview

Modularization issues occur when a module refers to a type or declaration that cannot be loaded from the same module. This can occur if the declaration is not re-exported correctly. The compiler should normally prevent this from happening; please file a bug report if you encounter this kind of error.

For debugging purposes these errors can be downgraded to remarks using `-Rmodule-recovery`.

