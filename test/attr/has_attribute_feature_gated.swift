// Consistency test that verifies that `hasAttribute` agrees with whether the
// language feature gating an attribute is enabled. This is meant to catch
// bugs in the overall handling of feature-gated attributes. Given how the
// implementation of `hasAttribute` is meta-programmed, it's less likely that
// we'd see bugs involving specific attributes here.
// 
// The attributes checked here come from DeclAttr.def when the test runs, so
// this test won't go stale when a feature graduates and its gating is removed;
// it will simply be no longer tested here.
//
// Each attribute is checked from both sides, which means no feature has to be
// enabled on the command line: a gated attribute whose feature is off must be
// invisible, and one whose feature is on by default must be visible. We have to
// avoid `-enable-experimental-feature` since we can't generate tests that
// dynamically require a `swift_feature_*` lit feature; the lit runner processes
// those requirement directives before we get to this point. As long as we have
// some `SUPPRESSIBLE_LANGUAGE_FEATURE`s that are associated with feature-gated
// attributes, we'll have coverage of both paths.

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// Let the C preprocessor flatten DeclAttr.def and TypeAttr.def into one line
// per entry, then generate a pair of `hasAttribute` checks for each gated
// attribute.
// RUN: %clang -E -P -x c -I %swift_src_root/include %t/decl-attrs.h -o %t/decl-attrs.txt
// RUN: %gyb -DDECL_ATTRS=%t/decl-attrs.txt %t/checks.swift.gyb -o %t/checks.swift
// RUN: %target-swift-frontend -typecheck %t/checks.swift

//--- decl-attrs.h

// Expands DeclAttr.def and TypeAttr.def into one line per entry:
//
//   ATTR: <spelling>, <class>, <behaviors>
//   FEATURE_GATED: <class>, <feature>
//   TYPE_ATTR: <spelling>
//
// Behaviors that hide an attribute from `hasAttribute` no matter which features
// are enabled expand to `HIDDEN`; keep them in sync with hasDeclAttribute() in
// lib/AST/Attr.cpp.

#define ConcurrencyOnly HIDDEN
#define DeclModifier HIDDEN
#define RejectByParser HIDDEN
#define SILOnly HIDDEN
#define UserInaccessible HIDDEN

#define DECL_ATTR(SPELLING, CLASS, REQUIREMENTS, BEHAVIORS, CODE) \
  ATTR: SPELLING, CLASS, BEHAVIORS
#define DECL_ATTR_FEATURE_REQUIREMENT(CLASS, FEATURE_NAME) \
  FEATURE_GATED: CLASS, FEATURE_NAME
#include "swift/AST/DeclAttr.def"

// `hasAttribute` answers for type attributes as well, and those have no feature
// requirements, so if a feature-gated decl attribute and a type attribute share
// the same spelling, hasAttribute will report it even if the feature isn't
// enabled. Thus, we have to gather those so we can exclude them later. SIL-only
// attributes, however, can be ignored.

#define SIL_TYPE_ATTR(SPELLING, CLASS)
#define TYPE_ATTR(SPELLING, CLASS) \
  TYPE_ATTR: SPELLING
#include "swift/AST/TypeAttr.def"

//--- checks.swift.gyb

%{
  # Read what the preprocessor made of the two .def files. See decl-attrs.h for
  # the shape of these lines.
  attributes = {}
  gatedClasses = {}
  typeAttributes = set()

  for lineNumber, line in enumerate(open(DECL_ATTRS), start=1):
    if not line.strip():
      continue
    kind, _, rest = line.partition(':')
    fields = [field.strip() for field in rest.split(',')]
    if kind == 'ATTR' and len(fields) == 3:
      spelling, attrClass, behaviors = fields
      attributes[attrClass] = (spelling, behaviors)
    elif kind == 'FEATURE_GATED' and len(fields) == 2:
      gatedClasses[fields[0]] = fields[1]
    elif kind == 'TYPE_ATTR' and len(fields) == 1:
      typeAttributes.add(fields[0])
    else:
      # Rather than skipping anything unrecognized, which would silently drop
      # attributes from the test, insist that every line is understood.
      raise Exception('%s:%d: %r does not look like anything decl-attrs.h '
                      'generates; have the .def files changed shape?'
                      % (DECL_ATTRS, lineNumber, line.strip()))

  # The attributes worth checking, paired with the feature that gates them.
  gatedAttributes = []
  for attrClass, feature in gatedClasses.items():
    spelling, behaviors = attributes[attrClass]
    if 'HIDDEN' in behaviors or spelling.startswith('_') or spelling in typeAttributes:
      continue
    gatedAttributes.append((spelling, feature))

  # If everything were filtered out, this test would generate no checks and pass
  # vacuously. We don't want to assert this because it's possible that some
  # future state of the language might not have any feature-gated attributes,
  # and we don't want to encourage removal of this test because we'd lose
  # coverage for any gated attributes added after that.
}%
% for spelling, feature in gatedAttributes:
#if !hasFeature(${feature}) && hasAttribute(${spelling})
#error("@${spelling} requires ${feature}, which is disabled here, so hasAttribute must not report it as available")
#endif

#if hasFeature(${feature}) && !hasAttribute(${spelling})
#error("@${spelling} requires ${feature}, which is enabled here, so hasAttribute must report it as available")
#endif

% end
