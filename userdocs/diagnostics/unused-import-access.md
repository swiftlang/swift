# Unused import access level (UnusedImportAccess)

## Overview

A `public` or `package` import was not referenced in any declarations at the corresponding access level.

A `public import` is expected to be used either by `public` declarations or by function bodies that are exposed to other modules using an attribute like `@inlinable`. A `package import` is expected to be used by `package` declarations. If the imported module is not referenced in any declarations with the corresponding access level then its access level can be reduced to avoid exposing unnecessary dependencies.

To resolve this warning, either lower the access level of the import or remove it if it is unused entirely. If the module is built with `-enable-upcoming-feature InternalImportsByDefault`, the access level can be omitted to make the import `internal`.
