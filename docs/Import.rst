I don't like our current import declaration design for a couple reasons:

- Importing all the exported symbols from a module is a bad default. It makes
  name clashes easy to introduce, and we don't currently provide decent ways to
  manage them. The potential for clashes encourages developers to defensively
  over-namespace their APIs by prefixing their exported names, a problem the
  module system should in theory solve for them.
- Importing a single item from a module looks the same as importing a
  submodule. ``import foo.bar`` could either import a single declaration
  ``bar`` or the entire contents of a module ``foo.bar``.
- We don't provide a good way to import multiple itemized symbols from a single
  module, short of a series of ``import foo.Bar; import foo.Bas; ...``
  declarations.
- It's useful to be able to specify local aliases for imported symbols. This is
  a nice way of disambiguating clashing imported names and providing short
  names for commonly used imports.

Here's a design I think will work well when we start to seriously support
modules. It's the model I implemented in Clay, which is very similar
to how Python's ``import``/``from ... import`` syntax works, but with what I
think are more reasonable resolution rules for name clashes, and without Python's
weirdness of two different syntactic forms for the same thing. It keeps our
current "give me everything" behavior easily accessible for when you want it,
but provides good support for explicit itemization of imports, and specifies
how the language should behave in the face of name clashes.

IMPORT SYNTAX
=============

The ``import`` statement gains three forms: import a module name only, import
a module's entire exported namespace, and import only selected symbols from a
module.

- ``import foo`` imports only the module name ``foo``, whose exported
  declarations can then be referenced with qualified names::

    import foo

    class A : foo.Bar { ... }

    class B : foo.Bas { ... }

- ``import foo.*`` imports all of the exported declarations from the module
  ``foo``::

    import foo.*

    class A : Bar { ... }

    class B : Bas { ... }

  The module name can also be used to qualify imported names.

- ``import foo.(Bar, ...)`` imports one or more selected declarations out of a
  module. The parens are required whether there are zero, one, or many itemized
  imports; this makes importing a submodule ``import foo.Bar`` syntactically
  distinct from importing a single symbol ``import foo.(Bar)``. Declarations
  not explicitly imported can still be referenced by qualified name::

    import foo.(Bar, Bas)

    // Imported symbols can be used unqualified
    class A : Bar { ... }

    class B : Bas { ... }

    // Other symbols require qualification
    class C : foo.Farr { ... }

  Additionally, modules and/or exported declarations can be imported with a
  local name specified using ``as <identifier>``::

    import foo as f

    // Local name 'f' refers to imported module 'foo'
    class A : f.Bar { ... }

    import alpha.(Omega as AO)
    import beta.(Omega as BO)

    // Local names AO and BO refer to imported symbols

    class A : AO { ... }

    class B : BO { ... }

    // Alias both the module name and an imported symbol from it
    import gamma.(Omega as GO) as g

  (Overloading the ``as`` keyword reads well but feels icky. Other possibilities
  include ``localName : exportedName`` or ``localName = exportedName``.)

RESOLVING NAME CLASHES WITH '.*' IMPORTS
========================================

Because the local names introduced by a ``.*`` import are implicit, it makes
sense not to raise errors about name clashes unless a clashing name is actually
used, and to resolve conflicts in favor of explicitly named declarations when
available. Thus, if two more modules are imported with the ``.*`` syntax, and
they export conflicting symbols, it is not an error, and non-conflicting names
from either imported module can be referenced unqualified. It is an error,
however, to reference conflicting names without a qualification::

  import abcde.* // abcde exports A, B, C, D, E
  import aeiou.* // aeiou exports A, E, I, O, U

  var b : B // OK, references abcde.B
  var i : I // OK, references aeiou.I
  var e : E // Error, ambiguous
  var e : abcde.E // OK, qualified reference to abcde.E

The conflicting name can also be explicitly imported with ``.(...)`` syntax
from one module or the other, in which case unqualified references refer to
that import::

  import abcde.* // abcde exports A, B, C, D, E
  import abcde.(E) // explicitly import E from abcde
  import aeiou.* // aeiou exports A, E, I, O, U

  var e : E // OK, references abcde.E

Local definitions may shadow names imported with ``.*`` syntax::

  import abcde.* // abcde exports A, B, C, D, E
  import aeiou.* // aeiou exports A, E, I, O, U

  class E { ... } // OK
  class U { ... } // OK

  var e : E // OK, references local definition E
  var u : U // OK, references local definition U

Symbols explicitly imported with ``.(...)`` may also shadow names implicitly
imported names::

  import abcde.* // abcde exports A, B, C, D, E
  import aeiou.* // aeiou exports A, E, I, O, U
  import qwerty.(E) // explicit import E shadows implicit imports

  var e : E // OK: references qwerty.E

RESOLVING NAME CLASHES WITH '.(...)' IMPORTS
============================================

Since the ``.(...)`` syntax fully spells out the names introduced to the local
namespace, it makes sense to be stricter in preventing name clashes.
It is thus an error to explicitly import the same name from multiple modules
using the itemized ``.(...)`` syntax, or to import two symbols with the same
local alias::

  import abcde.(E)
  import aeiou.(E) // Error, E already imported from abcde

  import abcde.(B as Z)
  import aeiou.(U as Z) // Error, B already imported as Z from abcde

However, you can import same-named symbols from multiple modules using
different local aliases::

  import abcde.(E)
  import aeiou.(E as E2)  // OK, local alias avoids clash

Defining a local definition with the same name as an explicitly imported symbol
is also an error::

  import abcde.(E)

  class E { } // Error, conflicts with explicitly imported symbol E

-Joe
