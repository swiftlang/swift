# Inconsistent import access level (InconsistentImportAccess)

## Overview

The same module is imported more than once in a single file with different access levels, and the lower-access import will be ignored.

When a module is imported multiple times in the same file, Swift uses the highest access level among all the imports. A redundant lower-access import has no effect and should be removed or consolidated.

To resolve this warning, remove the redundant lower-access import or adjust it to match the intended access level.
