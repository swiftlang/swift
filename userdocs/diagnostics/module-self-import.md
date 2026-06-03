# Module self-import (ModuleSelfImport)

## Overview

A file is importing the module it belongs to, which has no effect.

A module cannot import itself: the declarations in the current module are already in scope. The import statement will be ignored by the compiler.

To resolve this warning, remove the self-import.
