# Unsupported scoped import (UnsupportedScopedImport)

## Overview

A scoped import (e.g. `import func Foo.bar`) was used in a module interface, which is not yet supported.

Module interfaces (`.swiftinterface` files) record the public API of a module so it can be rebuilt from source. Scoped imports that refer to specific declarations within a module cannot yet be represented in the module interface format; the import is still recorded, but without the scoping qualifier.

To resolve this warning, replace the scoped import with a plain module import (e.g. `import Foo` instead of `import func Foo.bar`).
