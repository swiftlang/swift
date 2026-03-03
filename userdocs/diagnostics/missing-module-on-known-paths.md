# Missing module on known path from a dependency (MissingModuleOnKnownPaths)

Notes related to information about a missing module dependency.


## Overview

This diagnostic group covers notes related to displaying information about a missing module dependency which the compiler is able to locate as present on a search path found in a loaded Swift binary module, but which is not specified to the current compilation.

As one example of a potential missing module diagnostic, suppose an imported module `Foo` is resolved to a Swift binary module which itself depends on module `Bar` and was built with an additional search path where `Bar` is located, and suppose that the client which imports `Foo` does not specify this search path:

```
import Foo
```

The Swift compiler would emit a module-not-found error and a note to inform the user of the missing search path containing `Bar` which was found serialized in `Foo`'s binary Swift module:

```
error: Compilation search paths unable to resolve module dependency: 'Bar' [#MissingModuleOnKnownPaths]
note: 'Bar' can be found using a search path that was specified when building module 'Foo' ('<Search Path>'). This search path was not specified on the current compilation.
```

Some prior versions of the Swift compiler erroneously inherited search paths from loaded binary Swift modules and used them to resolve other, subsequently-encountered module dependencies. All search paths required to resolve direct and transitive module dependencies must be explicitly specified on the compiler invocation which will encounter these dependencies.
