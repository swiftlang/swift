# Name Translation from C to Swift

This document gives a high-level description of how C and Objective-C declarations are translated to Swift, with particular focus on how names are adjusted. It is not attempting to be a *complete* description of everything the compiler does except with regards to how *names* are transformed; even there, some special cases that only apply to Apple's SDKs have been omitted.

## Enums

1. Anonymous? Import elements as constants of the underlying type, *except* that Int is used for an inferred underlying type if all the cases fit in an Int32, since integer conversions are explicit in Swift.
2. `ns_error_domain` attribute? Import as an error struct (see below)
3. `flag_enum` attribute? Import as an option set struct (see below)
4. `enum_extensibility` attribute? Import as an `@objc` enum, whether open or closed
5. Otherwise, import as a RawRepresentable struct _without renaming constants_ (this is probably an oversight but would be source-breaking to change)

An enum declared as `typedef enum { … } MyEnum;` is not considered "anonymous"; the typedef name is used instead. This interpretation has precedent in the C++ linkage rules, which have a similar special case for typedefs of unnamed enums (C++17 [dcl.typedef]p9).

### `@objc` enums

```objc
enum TimeOfDay __attribute__((enum_extensibility(open))) : long {
  TimeOfDayMorning,
  TimeOfDayAfternoon,
  TimeOfDayNight,
  TimeOfDayEvening = TimeOfDayNight
};
// Most commonly seen as NS_ENUM and NS_CLOSED_ENUM.
```

```swift
@objc enum TimeOfDay: Int {
  init?(rawValue: Int)
  var rawValue: Int { get }

  case morning
  case afternoon
  case night

  static var evening: TimeOfDay { get }
}
```

- Enum name is enum name.
- Underlying type becomes raw type.
- Case names follow the rules for "enum-style prefix stripping" below.
- `init?(rawValue:)` currently does not check cases, but probably should for "closed" enums (Clang's equivalent of `@frozen`)

In order to deal with cases with the same underlying value, the importer picks a set of _canonical cases_ by walking the enum and recording the first *available* enumerator with a particular value. (Note that deprecated cases are still considered available unless they were deprecated for a platform older than Swift's earliest deployment targets, OS X 10.9 and iOS 7.) Any "non-canonical" cases are imported as computed properties instead.

### Error enum structs

```objc
enum VagueFailureCode __attribute__((ns_error_domain(VagueFailureDomain))) : long {
  VagueFailureBadness,
  VagueFailureWorseness,
  VagueFailureWorstness
};
// Most commonly seen as NS_ERROR_ENUM.
```

```swift
struct VagueFailure: Error {
  @objc enum Code: Int {
    init?(rawValue: Int)
    var rawValue: Int { get }

    case badness
    case worseness
    case worstness

    typealias ErrorType = VagueFailure
  }

  static var badness: VagueFailure.Code { get }
  static var worseness: VagueFailure.Code { get }
  static var worstness: VagueFailure.Code { get }

  static var errorDomain: String { get }
}
```

- Struct name is enum name, dropping the suffix "Code" if present.
- Original enum type is mapped to the nested "Code" enum using the rules for "`@objc` enums".
- Struct gets an `errorDomain` member populated from the `ns_error_domain` attribute.
- Struct gets aliases for the enum cases as static properties for easier use in `catch` clauses.

### Option set structs

```objc
enum PetsAllowed __attribute__((flag_enum)) : long {
  PetsAllowedNone = 0,
  PetsAllowedDogs = 1 << 0,
  PetsAllowedCats = 1 << 1
};
// Most commonly seen as NS_OPTIONS.
```

```swift
struct PetsAllowed: OptionSet {
  init(rawValue: Int)
  var rawValue: Int

  static var dogs: PetsAllowed { get }
  static var cats: PetsAllowed { get }
}
```

- Struct name is enum name.
- Underlying type becomes raw type.
- Cases are imported as static properties.
- Case names follow the rules for "enum-style prefix stripping" below.
- Cases with a raw value of `0` are **not imported** unless the case has an explicit `swift_name` attribute. (The intent is to use `[]` instead in Swift, representing "no options".)

### Enum-style prefix stripping

In C, enumerators (enum cases) aren't namespaced under their enum type, so their names have to make sense in a global context. The Swift compiler attempts to recognize several common idioms for ASCII-based names in order to translate these into idiomatic Swift members.

1. Collect all *available, non-deprecated* enum cases *without custom names.* If there are no such cases, collect all cases without custom names, whether available or not.

2. Find the common word-boundary prefix __CP__ of these cases. There is a word boundary after

    1. An underscore ("\_").
    2. A series of two or more uppercase ASCII characters and the suffix "s", "es", or "ies" (e.g. "URLs", "VAXes")...unless the last uppercase letter is "I" and the suffix is "s", in which case it's just as likely to be an acronym followed by "Is" (i.e. "URLIs" is treated as "URL Is").
    2. A series of two or more uppercase ASCII characters followed by an uppercase ASCII character and then a lowercase ASCII character ("XMLReader" becomes "XML Reader").
    3. A series of two or more uppercase ASCII characters followed by a non-ASCII-alphabetic character. ("UTF8" becomes "UTF 8")
    4. A series of two or more uppercase ASCII characters at the end of the string.
    5. An uppercase ASCII character and any number of non-ASCII-uppercase, non-underscore characters ("ContrivedExample" becomes "Contrived Example").
    6. Any number of non-ASCII-uppercase, non-underscore characters ("lowercase\_example" becomes "lowercase \_ example").

3. If __CP__ starts with "k" followed by an uppercase letter, or if it's *just* "k" and none of the cases have a non-identifier-start character immediately after the 'k', treat that as meaning "constant" and ignore it for the next step.

4. Find the common word-boundary prefix __EP__ of __CP__ and the type's original C name (rather than its Swift name).

5. If the next word of __CP__ after __EP__ is

    - the next word of the type's original C name minus "s" ("URL" vs. "URLs")
    - the next word of the type's original C name minus "es" ("Address" vs. "Addresses")
    - the next word of the type's original C name with "ies" replaced by "y" ("Property" vs. "Properties")

    add the next word of __CP__ to __EP__.

6. If the next word of __CP__ after __EP__ is an underscore ("MyEnum\_FirstCase" vs. "MyEnum"), add the underscore to __EP__.

7. If "k" was dropped from __CP__ in step 3, add it back to the front of __EP__.

8. For any case without a custom name, if it starts with __EP__, drop __EP__ from that case's name when importing it. (Deprecated or unavailable cases may not start with __EP__ depending on step 1.)

9. ASCII-lowercase the first word of the remaining name if it starts with an uppercase ASCII character.

    _There's a bug here where the special case for "Is" is missing, so "URLIs" will be lowercased to "urlis"._

## `swift_wrapper` typedefs

The `swift_wrapper` Clang attribute allows importing a typedef as a RawRepresentable struct type; it also causes global constants that use that type name to be imported as members of the new struct type.

```objc
typedef NSString * _Nonnull SecretResourceID __attribute__((swift_wrapper(struct)));

extern SecretResourceID const SecretResourceTreasureChest;
extern SecretResourceID const SecretResourceBankVault;

// Usually seen as NS_TYPED_ENUM and NS_TYPED_EXTENSIBLE_ENUM.
```

```swift
struct SecretResourceID: RawRepresentable, Hashable {
  typealias RawValue = String

  init(_ rawValue: String)
  init(rawValue: String)
  var rawValue: String { get }
}

extension SecretResourceID {
  static var treasureChest: SecretResource { get }
  static var bankVault: SecretResource { get }
}
```

- If the underlying type conforms to Equatable or Hashable, or is bridged to Objective-C, the wrapper type will too.
- The unlabeled `init(_:)` is only created when the Clang attribute is `swift_wrapper(struct)` rather than `swift_wrapper(enum)`.
- The constant names are imported using a simplified version of enum-style prefix stripping unless they have a custom name:

    1. If the constant name starts with "k" followed by an uppercase letter, treat that as meaning "constant" and ignore it for the next step.
    2. Find the common word-boundary prefix __P__ of the constant name and the typedef's original C name (rather than its Swift name).
    3. If "k" was dropped from the constant name in step 1, add it back to the front of __P__.
    4. Drop __P__ from the constant's name.
    5. ASCII-lowercase the first word of the remaining name if it starts with an uppercase ASCII character.

    _This default translation may result in a name that is not a valid identifier, in which case a custom name must be applied in order to reference the constant from Swift._

### NSNotificationName

On Apple platforms, whenever Foundation is imported, constants with the type "NSNotificationName" additionally have the suffix "Notification" stripped before performing the above rules unless they have a custom name. Global NSString constants whose name ends in "Notification" will also automatically be treated as if they were declared with the type NSNotificationName unless they have a custom name.

## More to come...
