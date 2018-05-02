//
// Scenario for this test (this actually occurs in the wild):
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//   1. A header for module M, in include-path, that defines @interface Foo
//      and a @protocol Bar that references Foo in one of its methods.
//
//   2. A symlink in include-path that links into M's (modular) header dir,
//      which effectively makes a "second definition" of the same interface
//      and protocol pair, but in a non-modular context.
//
//   3. A bridging header that imports the (non-modular)
//      header-defining-Foo-and-Bar
//
//   4. A swift file that imports M and implements Bar (thus referencing Foo).
//
//   5. Another swift file that does nothing special, but makes for a multi-file
//      compilation (requiring a merge-module step).
//
//
// What was previously going wrong (that we're testing for):
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//   1. Module M gets compiled into a PCM file with a local defn for M.Foo and
//      M.Bar referencing M.Foo.
//
//   2. The bridging header gets precompiled to a PCH with a non-modular
//      definition of Foo and Bar (because of the order we pass -Xcc includes
//      to the clang importer, they are preferred over frameworks).
//
//   3. Every time swift asks clang to import a defn -- modular or non-modular
//      -- clang reads _both_ definitions: first the one requested, then the
//      other one as a completion of its view of the set of defns for the
//      symbol. This wouldn't normally be a huge problem _except_ that in clang
//      rev 2ba19793, the rules for interfaces and protocols diverged:
//      interfaces became first-read-wins, protocols remained last-read-wins.
//
//   4. This means that when swift looks up (protocol) Bar, the non-modular
//      definition is found in the PCH, clang completes it with a modular defn,
//      and (since it's a protocol) last-read-wins: the modular M.Bar defn
//      sticks.  Swift then does a load-all-members on M.Bar and gets the
//      (directly-referenced) M.Foo defn, but since Foo is an interface, M.Foo
//      is the first-read and it sticks (ignoring the non-modular Foo read
//      immediately-after from the PCH). So Swift emits an XRef to M.Foo into
//      the partial .swiftmodule.
//
//   5. Later, the merge-modules step executes and reloads the .swiftmodule
//      file, reading the XRef to M.Foo. Unfortunately when it's run with a
//      textual bridging header, the textual, non-modular defn of Foo is fed
//      directly into the compiler by the preprocessor, so it sticks before any
//      lookups get started. This makes the modular XRef lookup of M.Foo fail.
//
//
// Why disabling bridging PCH fixes this
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// The first-read defn of Foo is fed into clang early, which sticks (as in
// the merge-modules step). This makes the serialized XRef have the form
// __ObjC.Foo, which succeeds when performed the second time in merge-modules.
//
//
// How to fix it:
// ~~~~~~~~~~~~~~
//
// One might hope that this can be fixed by having the merge-modules step take a
// PCH as well as other steps in the compilation. That unfortuantely only
// inverts the problem, which resurfaces in the definition-order of Bar itself:
// an XRef to __ObjC.Bar gets serialized, and then _it_ can't be found during
// merge-modules, because the *last-read* defn of Bar -- the modular one, M.Bar
// -- wins during lookup.
//
// So while we _do_ propose to have the merge-modules step use the PCH, we also
// put a patch into clang to apply the first-read-wins logic to protocols as
// well as interfaces.
//

// RUN: rm -rf %t
// RUN: mkdir -p %t/Headers/Simple
// RUN: ln -s %S/Inputs/frameworks/Simple.framework/Headers/Simple.h %t/Headers/Simple/Simple.h
// RUN: %target-build-swift -emit-module -module-name test -Xfrontend -enable-objc-interop -Xfrontend -disable-deserialization-recovery -v -F %S/Inputs/frameworks -Xcc "-I%t/Headers" -module-cache-path %t/clang-module-cache -import-objc-header %S/Inputs/pch-bridging-header-with-non-modular-import.h %S/Inputs/other.swift %s

import Simple
class Foo: SimpleProtocol {
    func foo(_ bar: SimpleInterface) {
    }
}
