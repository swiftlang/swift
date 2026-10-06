# Swift Optional in C++

This standalone benchmark measures the C++ wrappers emitted for Swift Optional.
The main benchmark suite currently drives Swift code importing C++, so it does
not build this C++ caller of a generated Swift header.

Use two complete Swift compiler builds and the same Clang compiler, SDK, target
and runtime for both. For a pull request, build its comparison base (usually the
merge base with `main`) and its head.
Each `swiftc` path must have `swift-frontend` beside it and `../lib/swift` resources.
The runner selects these explicitly, including when `swiftc` is a symlink.
Run on an otherwise idle machine after both compiler builds have finished:

```sh
python3 benchmark/interop/Optional/run.py \
  --before /path/to/baseline/bin/swiftc \
  --after /path/to/candidate/bin/swiftc \
  --clangxx /path/to/clang++ \
  --runtime-library-path /path/to/runtime/lib/swift/macosx \
  --output /tmp/optional-benchmark
```

The runner supports macOS and Linux with Clang and Python 3.9 or newer. On Linux,
use the `lib/swift/linux` runtime directory. `--sdk` and `--target` override the
host defaults. `--build-only` prepares the executables without timing them;
`--skip-build` runs the existing executables. Keep the same arguments in both
invocations. Without `--runtime-library-path`, each compiler's runtime paths are
used, which can introduce runtime differences into the comparison.

Swift uses `-O` and C++ uses `-O3 -DNDEBUG`, without LTO. Each process warms up each
case and reports the median of seven samples. The runner alternates the order of
the two executables across nine repetitions and reports the median of those
process medians. `--iterations` and `--repetitions` adjust the run length.
`build.json` and `run.json` record commands, compiler versions and target information;
`results.json` preserves each process's medians and wrapper size/alignment;
`summary.json` contains the comparison printed as CSV and the range of process
medians for each case. Retain the exact source revisions with the results.

Construction and copy cases include destruction. Assignment alternates existing
`some` and `none` values and reuses destination storage. String and Array copies
retain an existing payload, whose initial allocation is outside the measurement.
Large and over-aligned payloads exercise the heap fallback; plain Array copy is
an unchanged control. Vector traversal visits 65,536 alternating present and
absent values, with vector construction outside the measurement, and reports
nanoseconds per element. It includes the effect of the wrapper's footprint and
any indirection. A compiler barrier makes each constructed wrapper observable.

Allocation counts come from separate executables built with the opaque storage
allocator override. They count only C++ wrapper boxes per operation, excluding
Swift payload storage, metadata and vector allocations. Timed executables use
the normal allocator without counters. The traversal and assignment cases should
allocate no wrapper boxes on either revision.

The `std::optional<int64_t>` cases provide context for a C++ value whose payload
is stored inside the wrapper. Its layout is known to C++ at compile time, while
`swift::Optional<T>` uses Swift metadata and value witnesses. Their operations
and layouts differ, so their timings are not an interchangeable implementation
comparison. The Swift wrapper's size is also printed, since avoiding boxes can
increase the size of the wrapper itself.
