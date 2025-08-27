# Module not testable

Modules imported using `@testable import` must have been compiled using the `-enable-testing` compiler flag.

`@testable import` allows clients of a module to access `internal` API when writing tests. A `@testable import` will report an error if the imported module was not compiled with the `-enable-testing` compiler flag. Use of `-enable-testing` is recommended only in debug builds or other builds not intended for production use.