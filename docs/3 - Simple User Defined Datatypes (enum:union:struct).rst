.. @raise litre.TestsAreMissing
.. _SimpleUserDefinedDatatypes:

Swift Simple User Defined Datatypes (enum:union:struct)
=======================================================

The basic observation is that these datatypes are really important to be
efficient, and thus should be pass-by-value by default and inlined into larger
objects. This is the current behavior of NSRect and it works well. The basic
structure I think we should follow comes right from algebraic datatypes
(e.g. from the ML world), which combines enums/struct/union all into data
descriptor. Lets start with enums:

Enums
-----

Enums will be declared something like this::

  oneof DataSearchFlags { // Example stolen from CFDataSearchFlags
    Backwards,
    Anchored
  }

A major difference from C is that the elements of a 'oneof' don't get injected
into the global scope. This means that Backwards isn't valid in the global
scope, you have to use DataSearchFlags::Backwards or DataSearchFlags.Backwards
or something like that. This is good because you don't have to worry about your
enumerators clashing with other stuff in the global scope.

Given this declaration, you could do silly things like this::

  var x : DataSearchFlags // default initialized.
  var y = DataSearchFlags::Anchored
  var z : DataSearchFlags = DataSearchFlags::Anchored // redundant type specifier.

Of course, this is seriously over verbose, and it is also not typically how
enums get used in our APIs. The solution is to take advantage of the same
mechanics already in place for the "autoclosurification" which provides
context-sensitive type inference.  A new ":" operator defers name lookup + type
resolution until the context is resolved, allowing stuff like this::

  // Declaration, somewhere not in user code.
  func CFDataFind(...., compareOptions : DataSearchFlags)
  
  // Users see this.
  CFDataFind(a, b, c, :Anchored)

Compare this to the existing CF call, which looks like::

  CFDataFind(a, b, c, kCFDataSearchAnchored);

I think that the combination of deferred scoping operator plus context sensitive
name lookup will resolve a lot of over-verbosity issues without sacrificing
anything. Before talking about structs, lets talk about unions.

Unions
------

Because we want to be type-safe by default, we really care about discriminated
unions. Discriminated unions have a lot of power in ML style languages, allowing
truly elegant pattern matching and a lot of other great things. However, they
are so painful/verbose to do right that they are almost never used: Most uses of
unions in Cocoa.h are for "reinterpret some piece of data another way" (e.g. int
-> float) not for a proper discriminated union. Reinterpretations can be done
with appropriately named casts.

That said, there are some uses. Here is one (simplified) example from Cocoa::

  typedef UInt32 scalerStreamTypeFlag;
  enum {
    downloadStreamAction = 0,
    fontSizeQueryStreamAction = 1,
    encodingOnlyStreamAction = 2,
    prerequisiteQueryStreamAction = 3,
    prerequisiteItemStreamAction = 4,
    variationQueryStreamAction = 5
    };
  
  struct scalerStream {
    scalerStreamTypeFlag types;
    union {
      struct {
        const unsigned short * encoding;
        SInt32 * glyphBits;
        const char * name;
      } font;
      struct {
        SInt32 size;
        SInt32 list;
      } prerequisiteQuery;
      SInt32 prerequisiteItem;
      SInt32 variationQueryResult;
    } info;
  };

Note that the only real difference between a discriminated union and an enum is
that a union has types associated with the enumerators.  This is really
straight-forward, and builds off the existing tuple support we already have. The
example above would be declared like this (the types in [[]] brackets don't
exist yet in swift, use your imagination :-) ::

  oneof ScalerStream {
    Download,
    FontSizeQuery : (encoding : [[const unsigned short*]],
                     glyphBits : [[SInt32*]],
                     name : string),
    EncodingOnly,
    PrerequisiteQuery : (size : int, list : int),
    PrerequisiteItem : int,
    VariationQuery : int
  }

Building on what we have from the base expression/type definition, each
discriminator can have at most one type associated with it. In the fontsizequery
and prerequisite query cases, this type is a tuple with multiple elements. This
gives us access to the existing (and uniform) tuple initialization and
processing stuff.

With this declaration, you can use these like this::

  var x1 : ScalarStream = :Download
  var x2 = ScalarStream::Download // same as x1
  
  var y = ScalarStream::PrerequisiteItem 42
  x = :PrerequisiteQuery(.size = 2, .list = 42)
  x = :PrerequisiteQuery(2, 42)
  bar(:FontSizeQuery(.encoding = a, .glyphBits = b, .name = "foo"))
  bar(:FontSizeQuery(a, b, "foo"))

There would also be support for doing a "switch" style pattern matching dispatch
to get to the individual elements. A rough idea is something like this::

  switch (some_stream) {
    case EncodingOnly:
      ...
    case PrerequisiteItem x:
      handle(x)
      ...
    case FontSizeQuery(encoding, glyphBits, name):
      do_something_with(encoding + glyphBits, name)
      ...

There should also be an operator to check for discriminators and extract values,
etc. Basically we need a way to poke at the "isa" for the union.

Structs
-------

The last piece of this is the struct case, which is just a special case of a
union with exactly one discriminator. While structs are just a hacky special
case :-), they are important, because this is what most people think about. The
following would work::

  oneof CGRect {
    CGRect(origin : CGPoint, size : CGSize)
  }

  var x1 = CGRect::CGRect(myorigin, CGSize::CGSize(42, 123))
  var x2 = CGRect::CGRect(.size = CGSize::CGSize(.width = 42, .height=123), .origin = myorigin)

However, this seems like massive syntactic overkill. There are a couple ways to
handle this, but introducing a real "struct" keyword is probably the
simplest. This would give::

  struct CGRect { origin : CGPoint, size : CGSize }
  
  var x1 = CGRect(myorigin, CGSize(42, 123))
  var x2 = CGRect(.size = CGSize(.width = 42, .height=123), .origin = myorigin)
  var sum = x1.size.width + x1.size.height;

A struct declaration is just like a declaration of a oneof containing a single
element, plus it injects the (single) constructor into the global namespace. The
injected constructor is why "CGSize" works without requiring CGSize::CGSize or
:CGSize in an inferred context.  Internal to the compiler, this is just
de-sugared and handled uniformly with the more general oneof case, just like
'func' is de- sugared to 'var'.

In addition to injecting the constructor, a struct definition injects
definitions of accessor functions for each field into the containing scope. This
allows member access ("x1.size") is directly on structs through normal dot
syntax.

Other Stuff
-----------

Following the uniform syntax for variable and func definitions, oneof and struct
should allow attributes, e.g.::

  struct [packed] MyPoint { x : sometype1, y : sometype2 }

I don't have any specific plans for attributes here, but it could be useful when
we want a struct to exactly match the layout of a C type or a hardware
resource. It also allows us to specify that these are implicitly pass
by-reference if that ever becomes important. For example, that would allow us to
do something like this::

  struct [byref] MyList {
    data : int,
    next : MyList
  }

Without "byref" you'd get an error about MyList not allowed to be infinite
size. :-)

This would only be appropriate if you don't want to use an object for some
reason, which will always be "by-ref".









