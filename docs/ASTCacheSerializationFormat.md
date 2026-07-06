# Per-File AST Cache Serialization Format

> **Status:** Active development (branch `experimental-ast-cache`). No format
> stability guarantees. The `.swiftast` format is distinct from `.swiftmodule`
> and is allowed to break between compiler builds; the `.swiftmodule` format
> is **not** affected by any change documented here.

This document describes how a type-checked `SourceFile` AST is serialized to a
`.swiftast` cache file by the per-file AST cache
(`-experimental-ast-cache-dir <path>`), and how it is deserialized back into a
fresh `SourceFile` on a cache hit.

## 1. Design goals

1. **Per-file granularity.** Each `SourceFile` is serialized independently to
   its own `.swiftast` file, enabling incremental recompilation: an unchanged
   file is loaded from cache, a changed file is re-parsed and re-type-checked,
   and the new AST is written back to the cache.
2. **No swiftmodule format break.** The cache reuses the swiftmodule bitstream
   *record infrastructure* (layouts, abbreviations, the `Serializer`/
   `ModuleFile` machinery) but extends it with cache-only records appended at
   the end of the type/decl enums. Existing swiftmodule records are untouched,
   so a pre-built `Swift.swiftmodule` continues to load.
3. **Cross-file references without source parsing.** Same-module cross-file
   decls are emitted as cross-references (`XREF` path-piece records) and
   resolved at load time via the `CachedNominalDeclRegistry`, never via
   `lookupQualified` (which would trigger `ParseSourceFileRequest` on a cached
   — i.e. never-parsed — file and crash).
4. **Name-based associated types.** `DependentMemberType`s in cached files are
   serialized by name + owning protocol so the deserializer can resolve the
   `AssociatedTypeDecl` lazily via `ProtocolDecl::getAssociatedType(name)`
   without needing a `DeclID` that points into another file's decl table.

## 2. File layout

A `.swiftast` file is a length-prefixed binary container with three sections:

```
+--------------------------------------------------+
| ASTCacheKey header  (magic + version + hashes)   |
+--------------------------------------------------+
| Bitstream blob      (length-prefixed)             |
+--------------------------------------------------+
| SwiftDeps YAML blob (length-prefixed, may be empty)|
+--------------------------------------------------+
```

### 2.1 ASTCacheKey header

Defined in `include/swift/AST/TypeCheckedSnapshot.h`. Written by
`writeASTCacheFile` (`lib/Serialization/ASTCacheSerializer.cpp:142`) and parsed
by `SnapshotDeserializer::parseCacheHeader`
(`lib/Serialization/SnapshotDeserializer.cpp:256`).

| Field                       | Encoding              | Notes |
|-----------------------------|-----------------------|-------|
| `magic`                     | 9 bytes               | `"SWIFTAST\0"` (`SWIFTAST_MAGIC`) |
| `formatVersion`             | `uint32_t`            | Currently `1` (`SWIFTAST_FORMAT_VERSION`) |
| `compilerVersionHash`       | `uint32_t`            | Full Swift compiler version string hash |
| `sourceFileHash`            | length-prefixed string | SHA-256 of the source file content |
| `importedModulesHash`       | length-prefixed string | SHA-256 of imported modules (name + content) |
| `macroPluginsHash`          | length-prefixed string | SHA-256 of macro plugins (path + content) |
| `crossImportOverlaysHash`   | length-prefixed string | SHA-256 of cross-import overlay modules |
| `dependencyProvidesHash`    | length-prefixed string | SHA-256 of dependency "provides" (from `.swiftdeps`) |
| `importsBlob`               | length-prefixed string | Serialized `(module_name, import_attributes)` pairs; populates `SourceFile::Imports` on hit |
| `privateDiscriminator`      | length-prefixed string | The file's private discriminator |
| `overlaysBlob`              | length-prefixed string | `(overlay_name, declaring_name)` pairs |

> **Note:** `ASTCacheKey::langOptsHash` is present in the struct but is **not**
> written to the file in the current implementation. The wire format consists of
> the fixed-width fields above plus the eight length-prefixed strings, in the
> order listed. `parseCacheHeader` reads exactly these fields.

Length-prefixed string encoding: `uint32_t length` followed by `length` bytes
of UTF-8.

### 2.2 Bitstream blob

A length-prefixed blob containing a standard LLVM bitstream. Written by
`ASTCacheSerializer::writeToStream` (`ASTCacheSerializer.cpp:71`), which emits:

1. **BLOCKINFO block** (`writeBlockInfoBlock`) — registers abbreviation codes.
2. **MODULE_BLOCK** (`MODULE_BLOCK_ID`, version 2):
   - `writeHeader()` — swiftmodule header (signature `SWIFTMODULE_SIGNATURE`).
   - `writeInputBlock()` — imported module list.
   - `writeSIL(nullptr)` — no SIL is serialized for the cache.
   - `writeAST(DC)` — the type-checked AST.
   - `writeHiddenTypeLayoutsBlock()` — abbreviations for hidden type records.

The bitstream uses the **same record layouts** as `.swiftmodule` for all
existing records, plus the cache-only `DEPENDENT_MEMBER_NAMED_TYPE` record
(Section 4).

### 2.3 SwiftDeps YAML blob

A length-prefixed UTF-8 YAML blob matching the `.swiftdeps` format. Currently
written empty by `writeASTCacheFile` (`ASTCacheSerializer.cpp:168`). Reserved
for future dependency-graph embedding.

## 3. Cross-reference policy: `isDeclXRef`

The cache serializer (`ASTCacheSerializer`, `lib/Serialization/ASTCacheSerializer.cpp:44`)
overrides the base `Serializer::isDeclXRef` virtual (declared in
`lib/Serialization/Serialization.h:357`) to control which decls are inlined vs.
cross-referenced:

```cpp
bool isDeclXRef(const Decl *D) const override {
  const DeclContext *topLevel = D->getDeclContext()->getModuleScopeContext();
  if (topLevel->getParentModule() != M)
    return true;                 // cross-module: standard xref
  if (!SF || topLevel == SF || topLevel == SF->getSynthesizedFile())
    return false;                // local to this file: inline
  return true;                   // same-module cross-file: xref
}
```

| Decl location                           | `isDeclXRef` | Effect |
|-----------------------------------------|--------------|--------|
| Different module                        | `true`       | Standard `XREF` path-piece record |
| Same module, same file (or synthesized)  | `false`      | Inlined as a copy in the bitstream |
| Same module, different file             | `true`       | `XREF` path-piece record (cache-only resolution path) |

The third row is the cache-specific case. In a `.swiftmodule`, same-module
decls are always in the same file, so they are inlined. In the per-file cache,
two files of the same module are serialized separately; a decl in file B
referenced from file A must be xref'd, and the deserializer resolves it via the
`CachedNominalDeclRegistry` (Section 5) rather than `lookupQualified`.

> **History:** A previous version inlined `ProtocolDecl`s as copies (the
> "ProtocolDecl workaround") to avoid a `setWitness` assertion. The workaround
> was removed once registry-based resolution (Section 5) made xref'd protocols
> resolvable. All same-module cross-file decls — including protocols — are now
> xref'd.

## 4. New record: `DEPENDENT_MEMBER_NAMED_TYPE`

### 4.1 Motivation

`DependentMemberType` (e.g., `T.Assoc` where `T: P` and `P` has an associated
type `Assoc`) is serialized by the stock serializer as
`DEPENDENT_MEMBER_TYPE`, which stores a `DeclID` pointing at the
`AssociatedTypeDecl`. For the per-file cache this is problematic: the
`AssociatedTypeDecl` lives in another cached file's decl table, and resolving
its `DeclID` through the standard `getDeclChecked` path triggers
`lookupQualified` on that file, which crashes (Section 5).

The cache instead serializes the type **by name** plus the owning
`ProtocolDecl`, so the deserializer can resolve the `AssociatedTypeDecl`
lazily via `ProtocolDecl::getAssociatedType(name)`.

### 4.2 Record definition

Enum entry in `lib/Serialization/DeclTypeRecordNodes.def:130`:

```cpp
TYPE(DEPENDENT_MEMBER_NAMED)   // after TYPE(HIDDEN), before FIRST_DECL
```

> **Placement is load-bearing.** The `TYPE(...)` macro assigns sequential enum
> values. `DEPENDENT_MEMBER_NAMED` **must** be the last type entry — after
> `HIDDEN` and before `FIRST_DECL(TYPE_ALIAS, 50)`. Inserting it earlier would
> shift the numeric values of all subsequent type records and break the
> `.swiftmodule` format. Because it is appended at the end, existing
> `.swiftmodule` readers ignore it (they never encounter the record).

Layout in `lib/Serialization/ModuleFormat.h:1456`:

```cpp
TYPE_LAYOUT(DependentMemberNamedTypeLayout,
  DEPENDENT_MEMBER_NAMED_TYPE,
  TypeIDField,        // base type
  IdentifierIDField,  // associated type name
  DeclIDField         // protocol decl (for AssociatedTypeDecl resolution)
);
```

| Field              | Wire type   | ID space       | Meaning |
|--------------------|-------------|----------------|---------|
| `TypeIDField`      | `TypeID`    | Type table     | The base type (e.g., `T`) |
| `IdentifierIDField`| `IdentifierID` | Identifier table | The associated type name (e.g., `"Assoc"`) |
| `DeclIDField`      | `DeclID`    | Decl table     | The owning `ProtocolDecl` |

> **Implementation note:** `IdentifierIDField` and `DeclIDField` are the same
> underlying integer wire type (`ModuleFormat.h:86`), but they index different
> tables. The serializer emits them via `addDeclBaseNameRef` (identifier table)
> and `addDeclRef` (decl table) respectively.

### 4.3 Serialization

Gate: `Serializer::useNameBaseDependentMemberType()`
(`lib/Serialization/Serialization.h:362`) returns `false` by default and is
overridden to `true` by `ASTCacheSerializer` (`ASTCacheSerializer.cpp:67`).
Only the cache uses the named record; `.swiftmodule` continues to emit
`DEPENDENT_MEMBER_TYPE`.

`TypeSerializer::visitDependentMemberType` (`lib/Serialization/Serialization.cpp:6088`):

```cpp
if (S.useNameBasedDependentMemberType()) {
  const ProtocolDecl *proto = nullptr;
  if (auto *assocType = dependent->getAssocType())
    proto = assocType->getProtocol();
  DependentMemberNamedTypeLayout::emitRecord(
      S.Out, S.ScratchRecord, abbrCode,
      S.addTypeRef(dependent->getBase()),
      S.addDeclBaseNameRef(dependent->getName()),
      S.addDeclRef(proto));
} else {
  // stock path: DEPENDENT_MEMBER_TYPE with AssociatedTypeDecl DeclID
}
```

The abbreviation is registered alongside the stock type layouts in
`Serializer::registerDeclTypeAbbrevs` (`Serialization.cpp:6659`).

### 4.4 Deserialization

`DESERIALIZE_TYPE(DEPENDENT_MEMBER_NAMED_TYPE)` handler
(`lib/Serialization/Deserialization.cpp:8174`):

```cpp
decls_block::DependentMemberNamedTypeLayout::readRecord(
    scratch, baseID, assocTypeNameID, protoID);
auto assocTypeName = MF.getIdentifier(assocTypeNameID);
auto baseType = MF.getType(baseID);

if (protoID != 0) {
  auto protoResult = MF.getDeclChecked(protoID);
  if (auto *proto = dyn_cast<ProtocolDecl>(protoResult.get())) {
    if (auto *assocType = proto->getAssociatedType(assocTypeName))
      return DependentMemberType::get(baseType, assocType);
  }
}
// Fallback: name-only DependentMemberType (resolved lazily by type checking)
return DependentMemberType::get(baseType, assocTypeName);
```

The protocol `DeclID` is resolved through the normal `getDeclChecked` path
(safe because `ProtocolDecl`s are top-level nominals registered in the
`CachedNominalDeclRegistry`, Section 5). `getAssociatedType(name)` then walks
the protocol's associated type members and returns the matching
`AssociatedTypeDecl`, producing a fully-resolved `DependentMemberType` that the
requirement machine and constraint system can use directly.

If the protocol cannot be resolved (e.g., `protoID == 0` from a malformed
record, or the protocol decl failed to load), the handler falls back to
`DependentMemberType::get(base, name)`, which creates an unresolved type
resolved lazily during type checking — mirroring `ASTDemangler::createDependentMemberType`.

## 5. Cross-file decl resolution

### 5.1 The registry

`CachedNominalDeclRegistry` (in `ASTContext`, `include/swift/AST/ASTContext.h`)
maps `(name, kind, parentName, parentKind)` → `NominalTypeDecl*`. It is
populated eagerly during deserialization by
`SnapshotDeserializer::deserializeBitstream` (`SnapshotDeserializer.cpp:185`):

```cpp
for (auto *D : decls) {
  if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
    ctx.registerCachedNominalDecl(NTD, Identifier(), 0, true);
  }
}
```

Top-level nominals are registered with `parentName=Identifier()` and
`parentKind=0`. After all cache files are loaded, the registry contains every
top-level `Struct`, `Class`, `Enum`, and `Protocol` from every cached file in
the module.

### 5.2 Eager associated-type loading

Immediately after registering nominals, `deserializeBitstream` eagerly loads
associated type members for every `ProtocolDecl` (`SnapshotDeserializer.cpp:195`):

```cpp
for (auto *D : decls) {
  if (auto *PD = dyn_cast<ProtocolDecl>(D)) {
    (void)PD->getAssociatedTypeMembers();
  }
}
```

This forces the lazy member loaders to run for protocol associated types
during deserialization, so the requirement machine — which builds
`τ.[P:T]` introduction rules from `getAssociatedTypeMembers()` — sees a
non-empty list. Without this, `getReducedTypeParameter()` crashes.

### 5.3 The registry fast-path in `resolveCrossReference`

`ModuleFile::resolveCrossReference` (`lib/Serialization/Deserialization.cpp:2166`)
reads the first `XREF_*_PATH_PIECE` record and, in the stock path, calls
`lookupQualified` to find the top-level decl by name. For cached files,
`lookupQualified` constructs the `SourceLookupCache`, which calls
`getTopLevelItems()` on every file in the module — triggering
`ParseSourceFileRequest` on cached files that were never parsed, crashing the
compiler.

The cache fast-path (`Deserialization.cpp:2237`) intercepts same-module type
xrefs in cached files **before** `lookupQualified`:

```cpp
if (isType && FileContext && baseModule == getAssociatedModule()) {
  auto *SF = cast<FileUnit>(FileContext)->getParentSourceFile();
  if (SF && SF->LoadedFromAstCache && !privateDiscriminator) {
    for (uint8_t kind : {Struct, Class, Enum, Protocol}) {
      if (auto *found = getContext().lookupCachedNominalDecl(
              name.getIdentifier(), kind, Identifier(), 0)) {
        values.push_back(found);
        break;
      }
    }
    if (!values.empty())
      break;  // skip lookupQualified, fall through to remaining path pieces
  }
}
```

When the registry finds the nominal, `break` exits the `switch (recordID)`.
Because `values` is non-empty, the error-handling block at line 2396 is
skipped, and execution proceeds to the `while (--pathLen)` loop at line 2540,
which walks remaining path pieces via `nominal->lookupDirect()` and
`nominal->getExtensions()` — already safe (lazy member loading from the
bitstream, no source parsing).

**Why all four nominal kinds are tried:** the `XREF_TYPE_PATH_PIECE` record
encodes only the name, not the kind. The registry is keyed by `(name, kind,
...)`, so the lookup iterates `Struct`, `Class`, `Enum`, `Protocol`. Top-level
`TypeAliasDecl`s are handled separately via `allowTypeAliasXRef`.

**Fallback on registry miss:** if the registry does not contain the decl (e.g.,
the other cache file failed to load, or the decl is in a source-parsed file),
execution falls through to `lookupQualified`. For source-parsed files this is
correct (the file *is* parsed). For cross-file refs between cached files, a
miss indicates a load-ordering bug; `setAllowCompilerErrorsForCache` causes the
resulting `ErrorType` to be tolerated rather than crashing.

## 6. Two-pass `loadASTCache`

`CompilerInstance::loadASTCache` (`lib/Frontend/Frontend.cpp:2135`) loads cache
files in two passes to prevent `SourceLookupCache` construction from
source-parsing not-yet-loaded cached files:

**Pass 1 — read & pre-populate.** For each cacheable file, read the cache file
from disk. If present, call `SF->setTopLevelItems({})` to install an empty
`Items` vector. This prevents `SourceLookupCache::getTopLevelItems()` (called
by `lookupQualified` during another file's deserialization) from triggering
`ParseSourceFileRequest` on this file.

**Pass 2 — deserialize.** For each file with a cache entry, call
`SF->loadFromCache(...)`. On failure, call `SF->clearCacheForFailedLoad()`
(`include/swift/AST/SourceFile.h`) to reset `Items = std::nullopt`, allowing
normal parsing to proceed.

Without Pass 1, deserializing file A may trigger `lookupQualified` (via
`resolveCrossReference` or `getDeclChecked`) which constructs
`SourceLookupCache`. The cache constructor iterates *all* files calling
`getTopLevelItems()`; for files without `Items` set, this triggers
`ParseSourceFileRequest`, creating a **second** source-parsed `ProtocolDecl`
alongside the cached one. The requirement machine then picks up the
source-parsed copy (`HasLazy=0`, empty associated types) and crashes in
`getReducedTypeParameter`.

### 6.1 Cacheable files

A file is cacheable if (`Frontend.cpp:2176`):

- It is a `SourceFile` (not a serialized module file).
- It has a non-empty filename.
- It is **not** in script mode (`C4`: `TopLevelCodeDecl` is not serialized by
  the stock serializer, so caching a script-mode file would silently drop
  top-level executable code).

### 6.2 Save path

`saveASTCache` (`Frontend.cpp:2240`) writes cache files after type checking.
It skips:

- Files not at `ASTStage::TypeChecked`.
- Files already loaded from cache (`LoadedFromAstCache`).
- Script-mode files (`C4`).
- Files compiled with errors (`Diags.hadAnyError()`) — `ErrorType`s in cached
  ASTs crash subsequent deserializations.
- Files with invalid decls (common in batch/WMO mode where cross-batch refs
  may produce `ErrorType`s silently marked invalid).

The write is atomic: the file is written to `<path>.tmp.<pid>` and renamed.

## 7. Relationship to `.swiftmodule`

| Aspect                          | `.swiftmodule`                          | `.swiftast` (this format)                |
|---------------------------------|-----------------------------------------|-------------------------------------------|
| Granularity                     | Whole module                            | One file                                  |
| `isDeclXRef` (same-module)      | `false` (inline)                        | `true` for cross-file, `false` for local  |
| `DependentMemberType` record    | `DEPENDENT_MEMBER_TYPE` (DeclID)        | `DEPENDENT_MEMBER_NAMED_TYPE` (name+proto)|
| Decl resolution                 | In-module (decl table)                  | `CachedNominalDeclRegistry` for cross-file|
| Container                       | Bitstream only                          | ASTCacheKey header + bitstream + swiftdeps |
| Format stability                | Stable (bootstrap constraint)            | Unstable (in active development)          |
| `useNameBasedDependentMemberType` | `false`                              | `true`                                    |

The cache reuses the `Serializer` base class and its record-layout
infrastructure, so the bitstream encodes the same decl/type shapes. The only
new record is `DEPENDENT_MEMBER_NAMED_TYPE`, appended at the end of the type
enum so existing `.swiftmodule` readers never encounter it.

## 8. Debugging

`-debug-ast-cache` (parsed at `ArgsToFrontendOptionsConverter.cpp:82`) causes
`loadASTCache` and `saveASTCache` to print `AST cache: HIT`/`MISS` lines to
stderr, one per file. Example:

```
AST cache: MISS (no cache file) for Sources/_NIODataStructures/Heap.swift
AST cache: SAVED .../Heap.swift
...
AST cache: HIT for Sources/_NIODataStructures/Heap.swift
```

`-verify-ast-cache` (declared in `Options.td:1809`) is reserved for the
round-trip verification action (serialize → deserialize → JSON-diff); see the
plan in `local://ast-cache-format-plan.md`, Steps 10–13.

## 8.1. Remaining verify-ast-cache diffs (111 on rich test file)

The strict `compare()` in the verify script reports 111 diffs (down from 123).
Categories:

| Category | Count | Root cause | Status |
|----------|-------|-----------|--------|
| `range`/`loc` | 46 | Source locations not in decl record layouts (only `SwiftAttrAttr` serializes them via `writeSourceLocation`) | **Dropped** — needs `DECL_LOCS_BLOCK_ID` side block |
| `value_mismatch` (pointer IDs) | 61 | `decl_context` pointer IDs off-by-one — `ImportDecl` insertion shifts numbering. `ASTDumper` assigns `replaced-pointer-N` in first-seen order during JSON dump | **Dropped** — dump rendering artifact |
| `body` | 15 | Cache stores body *text* (source string via `InlinableBodyTextLayout`), not body *AST* (`brace_stmt` nodes). `ASTDumper` renders `body` as AST structure | **Dropped** — `ParseAbstractFunctionBodyRequest` returns `{}` for `BodyKind::Deserialized`; re-parsing via `parseAbstractFunctionBodyDelayed` requires `BodyKind::Unparsed` and a valid source range |
| `where_requirements` | 1 | `getTrailingWhereClause()` returns parsed where clause, not serialized. Generic signature IS serialized but includes inferred requirements (Copyable, Escapable) not in source where clause | **Dropped** — needs bitstream serialization of `TrailingWhereClause` |
| `implicit` on attrs | 2 | `has_storage_attr` vs `usable_from_inline_attr` swapped — synthesized attribute added at different position | **Dropped** — ordering depends on implicit vs explicit attrs |
| `accessors` array length | 1 | Missing/extra accessor in deserialized (`transparent_attr` propagated to accessor incorrectly) | **Dropped** — separate accessor attr propagation issue |

### Fixes applied

**Phase 1 (200 → 123):**

| Fix | Diffs cleared | How |
|-----|-------|-----|
| `interface_type` on `implicit_self_decl` | 15 | Eager self decl creation in `SnapshotDeserializer` with interface type from `DC->getSelfInterfaceType()` and specifier based on function kind |
| `requirement_signature` | 2 | Force-compute `ProtocolDecl::getRequirementSignature()` during deserialization |
| `inout` on self decls | 4 | `AccessorDecl` kind check (Set/Modify/Init = inout), `ConstructorDecl` handling |
| `enum_case_decl` reconstruction | ~20 cascade | Group consecutive `EnumElementDecl`s, insert `EnumCaseDecl` wrapper before each group, keep elements as direct members too |
| `import_decl` array alignment | ~10 | Reconstruct `ImportDecl` nodes from `importsBlob` during deserialization |

**Phase 2 (123 → 111) — TDD with lit tests and C++ unit tests:**

| Fix | Diffs cleared | How | Tests |
|-----|-------|-----|-------|
| `implicit` on patterns | 6 | Added `Pattern::clearImplicit()` (`Pattern.h:138`); recursively clears implicit flag on patterns within non-implicit PBDs in `Deserialization.cpp:4856`. Walker descends ParenPattern/TypedPattern/BindingPattern/TuplePattern. Synthesized (implicit) PBDs keep their patterns implicit | `test/ASTCache/pattern_implicit.swift`; `unittests/AST/PatternImplicitTests.cpp` (4 tests) |
| `generic_signature` | 1 | Force-compute `getGenericSignature()` on deserialized protocols in `SnapshotDeserializer.cpp:210` so ASTDumper renders `generic_signature` instead of `parsed_generic_params` | `test/ASTCache/generic_signature.swift`; `unittests/AST/GenericSignatureTests.cpp` (2 tests) |
| Synthesized conformances (`source_kind`) | 6 | Mark conformances as `Synthesized` when their protocol is not in the explicit inherits list (`SnapshotDeserializer.cpp:213`). Handles both `NominalTypeDecl` and `ExtensionDecl` inherits via `getInherited().getResolvedType(i)` | `test/ASTCache/synthesized_conformance.swift` |

### What's NOT fixable without extending the bitstream

The `range`, `body`, and `where_requirements` categories (62 diffs total)
require additional data in the cache bitstream that the swiftmodule format
doesn't store. The infrastructure exists:

- **`DECL_LOCS_BLOCK_ID`** (line 912 of `ModuleFormat.h`) — existing source
  location block from `.swiftsourceinfo`, could be added to the cache bitstream
- **`InlinableBodyTextLayout`** — body text is already stored; re-parsing it
  into AST would produce the `brace_stmt` nodes the dumper expects, but
  `ParseAbstractFunctionBodyRequest` returns `{}` for `BodyKind::Deserialized`
  and the request evaluator caches this empty result
- **Pattern record layouts** — the `implicit` fix (Phase 2) works around
  this by clearing implicit on non-implicit PBDs; the remaining 2 attr
  implicit diffs are attribute ordering issues

The `value_mismatch` pointer ID diffs (61) are a dump rendering artifact:
  `ASTDumper` assigns `replaced-pointer-N` in first-seen order during the JSON
  dump, and `ImportDecl` insertion shifts the numbering. Structural pointer ID
  normalization in the compare script could address this.

## 9. Key source files

| File | Role |
|------|------|
| `include/swift/AST/TypeCheckedSnapshot.h` | `ASTCacheKey`, `SWIFTAST_MAGIC`, `SWIFTAST_FORMAT_VERSION` |
| `include/swift/AST/SourceFile.h` | `clearCacheForFailedLoad()`, `LoadedFromAstCache`, `CachedModuleFile` |
| `include/swift/AST/Pattern.h` | `Pattern::clearImplicit()` (Phase 2 fix for pattern implicit-flag diffs) |
| `include/swift/AST/ProtocolConformance.h` | `NormalProtocolConformance::setSourceKindAndImplyingConformance()` (Phase 2 fix for synthesized conformance diffs) |
| `lib/Serialization/ASTCacheSerializer.cpp` | `ASTCacheSerializer` (`isDeclXRef`, `useNameBasedDependentMemberType`), `writeASTCacheFile` |
| `lib/Serialization/Serialization.h` | `Serializer` virtuals: `isDeclXRef`, `useNameBasedDependentMemberType`, `writeCrossReference` |
| `lib/Serialization/Serialization.cpp` | `visitDependentMemberType` branch, abbreviation registration |
| `lib/Serialization/ModuleFormat.h` | `DependentMemberNamedTypeLayout` |
| `lib/Serialization/DeclTypeRecordNodes.def` | `TYPE(DEPENDENT_MEMBER_NAMED)` enum entry |
| `lib/Serialization/Deserialization.cpp` | `DESERIALIZE_TYPE(DEPENDENT_MEMBER_NAMED_TYPE)`; `resolveCrossReference` registry fast-path |
| `lib/Serialization/SnapshotDeserializer.cpp` | `deserializeBitstream` (eager registry + associated-type load), `parseCacheHeader` |
| `lib/Frontend/Frontend.cpp` | `loadASTCache` (two-pass), `saveASTCache` |

## 10. Test files

### Lit tests (`test/ASTCache/`)

| File | Tests |
|------|-------|
| `basic.swift` | Cache MISS → SAVED → HIT cycle |
| `roundtrip.swift` | `-verify-ast-cache` invariant test (expects 0 diffs; currently fails due to remaining diffs) |
| `pattern_implicit.swift` | Pattern implicit-flag preservation (non-implicit PBDs + synthesized Hashable PBDs) |
| `generic_signature.swift` | Protocol generic signature force-computation |
| `synthesized_conformance.swift` | Synthesized vs explicit conformance source_kind |
| `cross_file_ref.swift` | Cross-file nominal resolution via registry |
| `protocol_cross_file.swift` | Cross-file protocol xref + associated types |
| `generic_cross_file.swift` | Cross-file generic type references |
| `extensions.swift` | Extension member serialization |
| `accessors.swift` | Accessor serialization |
| `wmo_crossref.swift` | WMO mode cross-references |
| `mixed_cache.swift` | Mixed cached + source-parsed files |
| `import_attrs.swift` | Import attribute preservation |
| `invalidation.swift` | Cache invalidation on source change |
| `default_args.swift` | Default argument serialization |
| `emit_object.swift` | `-emit-object` with cached AST |
| `fibonacci.swift` | Complex function bodies |
| `generics.swift` | Generic type serialization |

### Unit tests (`unittests/AST/`)

| File | Tests |
|------|-------|
| `PatternImplicitTests.cpp` | `Pattern::clearImplicit()` behavior: clears on named pattern, non-recursive, setImplicit re-arms, no-op on non-implicit (4 tests) |
| `GenericSignatureTests.cpp` | `GenericContext::hasComputedGenericSignature()`: false before set, true after (2 tests) |
