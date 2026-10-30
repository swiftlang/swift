// TEMPORARY diagnostic test. This is not meant to be merged.
//
// It fails on purpose (see the last RUN line) so that lit prints the full
// output of every command in the build log, not only the ones that fail.
//
// Notes on getting the output into the log:
//  - lit truncates every command's stdout/stderr at 10240 characters. The
//    diagnostics are therefore captured to files and printed in chunks of
//    9000 bytes, each by its own command. A short summary of the key lines
//    comes first in case the chunks are too long to read.
//  - lit aborts the whole script if a command can't be found (exit status
//    127), even with `|| true`. Commands that only need our own tools come
//    first; the LLVM binutils, which might not be on the PATH, come last.
//
// REQUIRES: OS=linux-android
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.2-abi-triple %S/Inputs/TypeLowering.swift -parse-as-library -emit-module -emit-library %no-fixup-chains -module-name TypeLowering -o %t/%target-library-name(TypesToReflect)
//
// RUN: echo "=== DIAG: ls ==="
// RUN: ls -l %t/%target-library-name(TypesToReflect) %platform-module-dir/%target-library-name(swiftCore) || true

// RUN: echo "=== DIAG: reflection-dump, both images: capture ==="
// RUN: env SWIFT_REFLECTION_DIAG=1 %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) %platform-module-dir/%target-library-name(swiftCore) -dump-type-lowering < %s > %t/both.out 2> %t/both.err.raw || true
// RUN: sed -E 's#(/[^/ ]+)+/##g' %t/both.err.raw > %t/both.err || true
// RUN: ls -l %t/both.out %t/both.err || true
// RUN: echo "=== DIAG: reflection-dump, libswiftCore only: capture ==="
// RUN: env SWIFT_REFLECTION_DIAG=1 %target-swift-reflection-dump %platform-module-dir/%target-library-name(swiftCore) -dump-type-lowering < %s > %t/core.out 2> %t/core.err.raw || true
// RUN: sed -E 's#(/[^/ ]+)+/##g' %t/core.err.raw > %t/core.err || true
// RUN: ls -l %t/core.out %t/core.err || true
// RUN: echo "=== DIAG: reflection-dump, test library only: capture ==="
// RUN: env SWIFT_REFLECTION_DIAG=1 %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) -dump-type-lowering < %s > %t/test.out 2> %t/test.err.raw || true
// RUN: sed -E 's#(/[^/ ]+)+/##g' %t/test.err.raw > %t/test.err || true
// RUN: ls -l %t/test.out %t/test.err || true
// RUN: echo "=== DIAG: reflection-dump, both images: KEY LINES ==="
// RUN: grep -E "WARNING|FAILED|NOT FOUND|FOUND|scanned|addReflectionInfo|addImage|scanELF|table version|: OK,|image #|loaded binary|symbol table|dynamic symbols|NO relocation" %t/both.err | head -c 9000 || true
// RUN: echo "=== DIAG: reflection-dump, libswiftCore only: KEY LINES ==="
// RUN: grep -E "WARNING|FAILED|NOT FOUND|FOUND|scanned|addReflectionInfo|addImage|scanELF|table version|: OK,|image #|loaded binary|symbol table|dynamic symbols|NO relocation" %t/core.err | head -c 9000 || true
// RUN: echo "=== DIAG: reflection-dump, test library only: KEY LINES ==="
// RUN: grep -E "WARNING|FAILED|NOT FOUND|FOUND|scanned|addReflectionInfo|addImage|scanELF|table version|: OK,|image #|loaded binary|symbol table|dynamic symbols|NO relocation" %t/test.err | head -c 9000 || true
// RUN: echo "=== DIAG: reflection-dump, both images: lowering results (stdout) ==="
// RUN: head -c 9000 %t/both.out || true
// RUN: echo "=== DIAG: reflection-dump, libswiftCore only: lowering results (stdout) ==="
// RUN: head -c 9000 %t/core.out || true
// RUN: echo "=== DIAG: reflection-dump, test library only: lowering results (stdout) ==="
// RUN: head -c 9000 %t/test.out || true
// RUN: echo "=== DIAG: reflection-dump, both images: FULL DIAGNOSTICS ==="
// RUN: echo "--- chunk 1/12 of both.err ---"
// RUN: tail -c +1 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 2/12 of both.err ---"
// RUN: tail -c +9001 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 3/12 of both.err ---"
// RUN: tail -c +18001 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 4/12 of both.err ---"
// RUN: tail -c +27001 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 5/12 of both.err ---"
// RUN: tail -c +36001 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 6/12 of both.err ---"
// RUN: tail -c +45001 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 7/12 of both.err ---"
// RUN: tail -c +54001 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 8/12 of both.err ---"
// RUN: tail -c +63001 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 9/12 of both.err ---"
// RUN: tail -c +72001 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 10/12 of both.err ---"
// RUN: tail -c +81001 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 11/12 of both.err ---"
// RUN: tail -c +90001 %t/both.err | head -c 9000 || true
// RUN: echo "--- chunk 12/12 of both.err ---"
// RUN: tail -c +99001 %t/both.err | head -c 9000 || true
// RUN: echo "=== DIAG: reflection-dump, libswiftCore only: FULL DIAGNOSTICS ==="
// RUN: echo "--- chunk 1/5 of core.err ---"
// RUN: tail -c +1 %t/core.err | head -c 9000 || true
// RUN: echo "--- chunk 2/5 of core.err ---"
// RUN: tail -c +9001 %t/core.err | head -c 9000 || true
// RUN: echo "--- chunk 3/5 of core.err ---"
// RUN: tail -c +18001 %t/core.err | head -c 9000 || true
// RUN: echo "--- chunk 4/5 of core.err ---"
// RUN: tail -c +27001 %t/core.err | head -c 9000 || true
// RUN: echo "--- chunk 5/5 of core.err ---"
// RUN: tail -c +36001 %t/core.err | head -c 9000 || true
// RUN: echo "=== DIAG: reflection-dump, test library only: FULL DIAGNOSTICS ==="
// RUN: echo "--- chunk 1/4 of test.err ---"
// RUN: tail -c +1 %t/test.err | head -c 9000 || true
// RUN: echo "--- chunk 2/4 of test.err ---"
// RUN: tail -c +9001 %t/test.err | head -c 9000 || true
// RUN: echo "--- chunk 3/4 of test.err ---"
// RUN: tail -c +18001 %t/test.err | head -c 9000 || true
// RUN: echo "--- chunk 4/4 of test.err ---"
// RUN: tail -c +27001 %t/test.err | head -c 9000 || true

// What the linker produced, according to LLVM's readelf: the section list (is
// .symtab there? are there swift5_* sections? what type are the relocation
// sections?), the program headers, the dynamic table, and the dynamic symbols.
// RUN: echo "=== DIAG: llvm-readelf of libswiftCore ==="
// RUN: %llvm-readelf -h -S -d %platform-module-dir/%target-library-name(swiftCore) > %t/core.readelf || true
// RUN: echo "--- chunk 1/3 of core.readelf ---"
// RUN: tail -c +1 %t/core.readelf | head -c 9000 || true
// RUN: echo "--- chunk 2/3 of core.readelf ---"
// RUN: tail -c +9001 %t/core.readelf | head -c 9000 || true
// RUN: echo "--- chunk 3/3 of core.readelf ---"
// RUN: tail -c +18001 %t/core.readelf | head -c 9000 || true
// RUN: echo "=== DIAG: llvm-readelf of TypesToReflect ==="
// RUN: %llvm-readelf -h -S -d %t/%target-library-name(TypesToReflect) > %t/test.readelf || true
// RUN: echo "--- chunk 1/3 of test.readelf ---"
// RUN: tail -c +1 %t/test.readelf | head -c 9000 || true
// RUN: echo "--- chunk 2/3 of test.readelf ---"
// RUN: tail -c +9001 %t/test.readelf | head -c 9000 || true
// RUN: echo "--- chunk 3/3 of test.readelf ---"
// RUN: tail -c +18001 %t/test.readelf | head -c 9000 || true
// RUN: echo "=== DIAG: program headers ==="
// RUN: %llvm-readelf -l %platform-module-dir/%target-library-name(swiftCore) | head -c 9000 || true
// RUN: echo "=== DIAG: dynamic symbols mentioning swift5 / reflection ==="
// RUN: %llvm-readelf --dyn-syms %platform-module-dir/%target-library-name(swiftCore) | grep -E "swift5|reflection" | head -c 9000 || true
// RUN: %llvm-readelf --dyn-syms %t/%target-library-name(TypesToReflect) | grep -E "swift5|reflection" | head -c 9000 || true
// RUN: echo "=== DIAG: first relocations of libswiftCore ==="
// RUN: %llvm-readelf -r %platform-module-dir/%target-library-name(swiftCore) | head -c 9000 || true
// RUN: echo "=== DIAG: first relocations of TypesToReflect ==="
// RUN: %llvm-readelf -r %t/%target-library-name(TypesToReflect) | head -c 9000 || true

// RUN: not echo "=== DIAG: done; failing on purpose so that lit prints all of the output above ==="

// Typerefs to lower: stdlib types, builtin types, and types local to the test
// library.
Si
Bp
Bi64_
12TypeLowering11BasicStructV
12TypeLowering10SimpleEnumO
12TypeLowering10SimpleEnumOBW
