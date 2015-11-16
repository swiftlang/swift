:orphan:

``@_transparent``
=================

Semantically, ``@_transparent`` means something like "treat this operation as
if it were a primitive operation". The name is meant to imply that both the
compiler and the compiled program will "see through" the operation to its
implementation.

This has several consequences:

- Any calls to a function marked ``@_transparent`` MUST be inlined prior to
  doing dataflow-related diagnostics, even under ``-Onone``. This may be
  necessary to *catch* dataflow errors.

- Because of this, a ``@_transparent`` function is inherently "fragile", in
  that changing its implementation most likely will not affect callers in
  existing compiled binaries.

- Because of this, a ``@_transparent`` function MUST only reference public
  symbols, and MUST not be optimized based on knowledge of the module it's in.
  [This is not currently implemented or enforced.]

- Debug info SHOULD skip over the inlined operations when single-stepping
  through the calling function.

This is all that ``@_transparent`` means.


When should you use ``@_transparent``?
--------------------------------------

- Does the implementation of this function ever have to change? Then you can't
  allow it to be inlined.

- Does the implementation need to call private things---either true-``private``
  functions, or ``internal`` functions that might go away in the next release?
  Then you can't allow it to be inlined. (Well, you can for now for
  ``internal``, but it'll break once we have libraries that aren't shipped with
  apps.)

- Is it okay if the function is *not* inlined? You'd just prefer that it were?
  Then you should use [the attribute we haven't designed yet], rather than
  ``@_transparent``. (If you really need this right now, try
  ``@inline(__always)``.)

- Is it a problem if the function is inlined even under ``-Onone``? Then you're
  really in the previous case. Trust the compiler.

- Is it a problem if you can't step through the function that's been inlined?
  Then you don't want ``@_transparent``; you just want ``@inline(__always)``.

- Is it okay if the inlining happens after all the dataflow diagnostics? Then
  you don't want ``@_transparent``; you just want ``@inline(__always)``.

If you made it this far, it sounds like ``@_transparent`` is the right choice.


Current implementation limitations
----------------------------------

- We don't have a general ``@inlineable`` attribute for functions that *allows*
  inlining but doesn't *require* it.

- As mentioned above, we don't enforce that inlineable things only refer to
  public symbols. rdar://problem/22666548

- We also don't keep from optimizing based on implementation details of the
  current module. [No Radar yet.]

- If you have local types in your inlineable function, serialization falls
  over. (As does textual SIL.) rdar://problem/17631278

- When compiling in non-single-frontend mode, SIL is generated for each file
  but then thrown away in the "merge modules" step. So none of it is inlineable
  for external callers. (Currently, ``-whole-module-optimization`` is
  equivalent to ``-force-single-frontend-invocation``.) rdar://problem/18913977

- Similarly, when compiling in non-single-frontend mode, no SIL is generated for
  any functions but those in the primary file (for each frontend invocation),
  including ``@inline(__always)`` and ``@_transparent`` functions. This is
  semantically a bug. rdar://problem/15366167
