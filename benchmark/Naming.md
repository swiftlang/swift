# Naming Convention

Historically, benchmark names in the Swift Benchmark Suite were derived from the
name of the `runFunction`, which by convention started with prefix `run_`,
followed by the benchmark name. Therefore most of the legacy benchmark names
conform to the [`UpperCamelCase`](http://bit.ly/UpperCamelCase) convention.
After introduction of
[`BenchmarkInfo`](https://github.com/apple/swift/pull/12048)
to describe the benchmark metadata, names can be any string. To create more
cohesive and well structured system, names of newly added benchmarks should meet
the following set of requirements:

<ul>
<li>
<!-- The <li> content with <details> is pre-formatted as HTML, to work around
Markdown renderer interrupting the paragraph, which creates an ugly gap. -->
<strong>Letters, numbers and dots.</strong> Start with short unique name in
<code>UpperCamelCase</code>.
For a family of benchmarks, separate the name components with periods.
<details>

Very long compound names using `UpperCamelCase` are hard to read. Use `.` to
increase readability and structure.

Prefer unique and creative name to nondescript generic term, unless the
benchmark is testing individual method on a concrete type.

````
⛔️ Dictionary2
✅ AngryPhonebook
✅ Dictionary.AnyHashable.String.update
✅ Array.append.Array.Int
````

Benchmark names are used to run individual tests when passed as command line
arguments to the benchmark driver. Special characters that could be interpreted
by the shell would require quoting. Stick to ASCII letters, numbers and period.
Exceptionally:

* Use **`-`** only to denote control flow constructs like `for-in` or `if-let`.
* Use **`!`** and **`?`** for optional types, conditional or forced downcasting
and optional chaining.

````
✅ OCB.NSArray.AnyObject.as?.Array.NSString
✅ OCB.NSArray.AnyObject.as!.Array.String
✅ Array.append.Array.Int?
✅ Flatten.Array.Tuple4.for-in.reserved
````
</details><p><!-- spacer --></p></li>
<li>
<strong>Use groups and variants</strong> to structure the benchmark family by
degrees of freedom (e.g. different types implementing a protocol, value vs.
reference types etc.). Use <strong>numbered suffixes</strong> to denote
differently sized variants.
<details>

Benchmarks in a family can be grouped by the tested operation, method or varied
by types and different workload sizes. It might be necessary to abbreviate some names to fit the size limit, based on the longest combination. Choose consistent names for the components throughout all members in the family, to allow for relative comparison across the different axis of variation.

````
✅ Seq.dropFirst.Array
✅ Seq.dropLast.Range.lazy
✅ Seq.dropWhile.UnfoldSeq
✅ Seq.prefix.AnySeq.RangeIter.lazy
✅ Seq.prefixWhile.AnyCol.Array
✅ Seq.suffix.AnySeq.UnfoldSeq.lazy

✅ Existential.Array.ConditionalShift.Ref1
✅ Existential.Array.Mutating.Ref2
✅ Existential.Array.method.1x.Ref3
✅ Existential.Array.method.2x.Ref4
✅ Existential.Array.Shift.Val0
✅ Existential.MutatingAndNonMutating.Val1
✅ Existential.Mutating.Val2
✅ Existential.method.1x.Val3
✅ Existential.method.2x.Val4
✅ Existential.Pass2.method.1x.Ref1
✅ Existential.Pass2.method.2x.Ref2

✅ Set.isSubset.Int25
✅ Set.symmetricDifference.Int50
````

</details><p><!-- spacer --></p></li>
<li>
<strong>Groups and types are <code>UpperCase</code>, methods are
<code>lowerCase</code>.</strong>
<details>

Use periods to separate the name components in variants derived from specialised
generic types or significant method chains.

````
⛔️ InsertCharacterStartIndex
⛔️ InsertCharacterTowardsEndIndexNonASCII
````

There's no need to be literal with type names. **Be descriptive**:

````
✅ Flatten.Array.Tuple4.lazy.flatMap
✅ String.insert.ASCIIChar.StartIndex
✅ String.insert.EmojiChar.NearEnd
````

</details><p><!-- spacer --></p></li>
<li>
<strong>Keep it short.</strong> 40 characters at most. Abbreviate if necessary!
<details>

Benchmarking results are reported on GitHub and very long names are causing
horizontal table scrolling which unfortunately obscures the columns with actual
measurements. Fixed upper size limit also helps with the formatted console
output, when measuring locally. *It is more important for benchmark's name to be
unique and short, than overly descriptive.*

Prefer concise names for potential benchmark family extensions. Leave out the
nested types from variants if they aren't strictly necessary for disambiguation.
If there's potentially valuable future variant, like testing `ContinuousArray`,
keep the `Array` now, allowing for addition of `ContArr` variants later.

Use **`Val`** and **`Ref`** as short descriptors for variants that compare value
types (`struct`, `Int`) with reference types (often named with `Class` in the
legacy-style).
Prefer **`Char`** to `Character`, which can be combined with codepage or
language prefix/suffix when necessary (`ASCIIChar`). For benchmarks that measure
`String`'s Unicode performance for various languages, use
[two letter codes](https://en.wikipedia.org/wiki/ISO_639-1) instead of spelling
out the whole language names.

In a pinch, even C-style naming with two letter prefixes `OC` (for Objective-C)
or abbreviations like  `Str` and `Arr` are OK, if it helps to fit a system with
descriptive names into 40 characters:

````
✅ CharCount.ContArr.Str.kr
✅ Seq.prefixWhile.AnySeq.UnfoldSeq.lazy
````

As a last resort, use *numbered suffixes* to disambiguate between benchmarks
with minor implementation variations.

</details></li>
</ul>

Technically, the benchmark's name must match the following regular expression:
`[A-Z][a-zA-Z0-9\-\.!?]+`
