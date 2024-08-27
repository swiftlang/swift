// RUN: %target-typecheck-verify-swift

#if !hasAttribute(dynamicCallable)
BOOM
#endif

#if hasAttribute(fortran)
BOOM
#endif

#if hasAttribute(cobol)
this is unparsed junk // expected-error{{consecutive statements on a line must be separated by}}
#endif

#if hasAttribute(optional)
ModifiersAreNotAttributes
#endif

#if hasAttribute(__raw_doc_comment)
UserInaccessibleAreNotAttributes
#endif

#if hasAttribute(17)
// expected-error@-1:5 {{single unlabeled argument for the attribute}}
#endif

#if !hasAttribute(escaping)
#error("type attributes are valid")
#endif

#if !hasAttribute(convention)
#error("type attributes are valid")
#endif

#if !hasAttribute(retroactive)
#error("type attributes are valid")
#endif

#if hasAttribute(in_guaranteed)
#error("SIL type attributes are invalid")
#endif

#if hasAttribute(opened)
#error("SIL type attributes are invalid")
#endif
