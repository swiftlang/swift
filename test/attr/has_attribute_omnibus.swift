// Consistency test that verifies that every user-facing declaration attribute
// is visible to `hasAttribute`. The list of attributes comes from DeclAttr.def
// when the test runs, so newly added attributes are covered automatically.
//
// The rule this enforces: an attribute whose spelling does not begin with an
// underscore (with existing exceptions noted below) is assumed to be part of
// the language's official surface, so `hasAttribute` must return true so that
// users can query it. Forgetting to drop `UserInaccessible` when an underscored
// attribute is promoted to an official one is an easy mistake to make, and it
// silently breaks feature detection for clients.

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// Let the C preprocessor flatten DeclAttr.def into one line per attribute,
// then generate a `hasAttribute` check for each user-facing one.
// RUN: %clang -E -P -x c -I %swift_src_root/include %t/decl-attrs.h -o %t/decl-attrs.txt
// RUN: %gyb -DDECL_ATTRS=%t/decl-attrs.txt %t/checks.swift.gyb -o %t/checks.swift
// RUN: %target-swift-frontend -typecheck %t/checks.swift

//--- decl-attrs.h

// Expands DeclAttr.def into one line per declaration attribute:
//
//   ATTR: <spelling>, <class>, <behaviors>
//   FEATURE_GATED: <class>
//
// Behaviors that keep an attribute out of `hasAttribute` for reasons unrelated
// to whether it is user-facing are expanded to `HIDDEN`; keep them in sync with
// hasDeclAttribute() in lib/AST/Attr.cpp. `UserInaccessible` is deliberately
// not listed here, because a non-underscored attribute that is still marked
// user-inaccessible is what this test is looking for.

#define ConcurrencyOnly HIDDEN
#define DeclModifier HIDDEN
#define RejectByParser HIDDEN
#define SILOnly HIDDEN

#define DECL_ATTR(SPELLING, CLASS, REQUIREMENTS, BEHAVIORS, CODE) \
  ATTR: SPELLING, CLASS, BEHAVIORS

#define DECL_ATTR_FEATURE_REQUIREMENT(CLASS, FEATURE_NAME) \
  FEATURE_GATED: CLASS

#include "swift/AST/DeclAttr.def"

//--- checks.swift.gyb

%{
  # This list contains existing attributes that are not officially supported,
  # but which do not start with an underscore and are still marked
  # UserInaccessible. Adding more to this list is RISKY because this test will
  # never be able to catch a mistake if the attribute is promoted to supported
  # in the future but UserInaccessible is not removed.
  #
  # New attributes that are not yet publicly supported that wish to be
  # UserInaccessible should either have a name that starts with an undescore
  # or be feature-gated.
  preexistingViolations = [
    'LLDBDebuggerFunction',
    'extractConstantsFromMembers',
    'reparentable',
    'sensitive',
    'unsafe_no_objc_tagged_pointer',
  ]

  attributes = []

  # Feature-gated attributes will be ignored because this test does not enable
  # any features, so `hasAttribute` will return false for them.
  featureGatedAttrs = set()

  for lineNumber, line in enumerate(open(DECL_ATTRS), start=1):
    if not line.strip():
      continue
    kind, _, rest = line.partition(':')
    fields = [field.strip() for field in rest.split(',')]
    if kind == 'ATTR' and len(fields) == 3:
      attributes.append(fields)
    elif kind == 'FEATURE_GATED' and len(fields) == 1:
      featureGatedAttrs.add(fields[0])
    else:
      # Rather than skipping anything unrecognized, which would silently drop
      # attributes from the test, insist that every line is understood.
      raise Exception('%s:%d: %r does not look like anything decl-attrs.h '
                      'generates; has DeclAttr.def changed shape?'
                      % (DECL_ATTRS, lineNumber, line.strip()))

  def isExpectedToBeVisible(spelling, attrClass, behaviors):
    return (not spelling.startswith('_')
            and spelling not in preexistingViolations
            and attrClass not in featureGatedAttrs
            and 'HIDDEN' not in behaviors)

  # A handful of spellings have more than one definition (`rethrows` is both a
  # modifier and an attribute, for example). If any of them would be hidden,
  # `hasAttribute` may well pick that one, so leave the spelling out entirely.
  visibleAttributes = sorted(
      set(spelling for spelling, attrClass, behaviors in attributes
          if isExpectedToBeVisible(spelling, attrClass, behaviors))
      - set(spelling for spelling, attrClass, behaviors in attributes
            if not isExpectedToBeVisible(spelling, attrClass, behaviors)))

  # If the filtering above ever excluded everything (e.g., if DeclAttr.def
  # changed in some way that we didn't expect), then this test would generate no
  # checks and pass vacuously. Use `@available` as a canary; it's always
  # accessible and not going anywhere.
  assert 'available' in visibleAttributes, \
      'expected @available to be checked; the filtering above is broken'
}%
% for spelling in visibleAttributes:
#if !hasAttribute(${spelling})
#error("@${spelling} is not visible to hasAttribute. If it is meant to be user-facing, remove `UserInaccessible` from its entry in DeclAttr.def. Otherwise, prepend an underscore or make it feature-gated.")
#endif

% end
