# Modularization errors (ModularizationIssue)

Errors detected when loading a module.

## Overview

Emits a errors when a module's public API refers to a declaration or type that is not found in the module. This could be because it's not public, or because it's imported without being reexported. The compiler should normally prevent this from happening; please file a bug report if you encounter one of these errors.

For debugging purposes these errors can be downgraded to remarks using -Rmodule-recovery.
