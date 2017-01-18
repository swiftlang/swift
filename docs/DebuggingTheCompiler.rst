:orphan:

.. highlight:: none

Debugging the Swift Compiler
============================

.. contents::

Abstract
--------

This document contains some useful information for debugging the
Swift compiler and Swift compiler output.

Printing the Intermediate Representations
-----------------------------------------

The most important thing when debugging the compiler is to examine the IR.
Here is how to dump the IR after the main phases of the Swift compiler
(assuming you are compiling with optimizations enabled):

#. **Parser**. To print the AST after parsing::

    swiftc -dump-ast -O file.swift

#. **SILGen**. To print the SIL immediately after SILGen::

    swiftc -emit-silgen -O file.swift

#. **Mandatory SIL passes**. To print the SIL after the mandatory passes::

    swiftc -emit-sil -Onone file.swift

  Well, this is not quite true, because the compiler is running some passes
  for -Onone after the mandatory passes, too. But for most purposes you will
  get what you want to see.

#. **Performance SIL passes**. To print the SIL after the complete SIL
   optimization pipeline::

    swiftc -emit-sil -O file.swift

#. **IRGen**. To print the LLVM IR after IR generation::

    swiftc -emit-ir -Xfrontend -disable-llvm-optzns -O file.swift

4. **LLVM passes**. To print the LLVM IR after LLVM passes::

    swiftc -emit-ir -O file.swift

5. **Code generation**. To print the final generated code::

    swiftc -S -O file.swift

Compilation stops at the phase where you print the output. So if you want to
print the SIL *and* the LLVM IR, you have to run the compiler twice.
The output of all these dump options (except ``-dump-ast``) can be redirected
with an additional ``-o <file>`` option.


Debugging on SIL Level
~~~~~~~~~~~~~~~~~~~~~~

Options for Dumping the SIL
```````````````````````````

Often it is not sufficient to dump the SIL at the beginning or end of the
optimization pipeline.
The SILPassManager supports useful options to dump the SIL also between
pass runs.

The option ``-Xllvm -sil-print-all`` dumps the whole SIL module after all
passes. Although it prints only functions which were changed by a pass, the
output can get *very* large.

It is useful if you identified a problem in the final SIL and you want to
check which pass did introduce the wrong SIL.

There are several other options available, e.g. to filter the output by
function names (``-Xllvm -sil-print-only-function``/``s``) or by pass names
(``-Xllvm -sil-print-before``/``after``/``around``).
For details see ``PassManager.cpp``.

Dumping the SIL and other Data in LLDB
``````````````````````````````````````

When debugging the Swift compiler with LLDB (or Xcode, of course), there is
even a more powerful way to examine the data in the compiler, e.g. the SIL.
Following LLVM's dump() convention, many SIL classes (as well as AST classes)
provide a dump() function. You can call the dump function with LLDB's
``expression --`` or ``print`` or ``p`` command.

For example, to examine a SIL instruction::

    (lldb) p Inst->dump()
    %12 = struct_extract %10 : $UnsafeMutablePointer<X>, #UnsafeMutablePointer._rawValue // user: %13

To dump a whole function at the beginning of a function pass::

    (lldb) p getFunction()->dump()

SIL modules and even functions can get very large. Often it is more convenient
to dump their contents into a file and open the file in a separate editor.
This can be done with::

    (lldb) p getFunction()->dump("myfunction.sil")

You can also dump the CFG (control flow graph) of a function::

    (lldb) p Func->viewCFG()

This opens a preview window containing the CFG of the function. To continue
debugging press <CTRL>-C on the LLDB prompt.
Note that this only works in Xcode if the PATH variable in the scheme's
environment setting contains the path to the dot tool.

Debugging and Profiling on SIL level
````````````````````````````````````

The compiler provides a way to debug and profile on SIL level. To enable SIL
debugging add the front-end option -gsil together with -g. Example::

    swiftc -g -Xfrontend -gsil -O test.swift -o a.out

This writes the SIL after optimizations into a file and generates debug info
for it. In the debugger and profiler you can then see the SIL code instead of
the Swift source code.
For details see the SILDebugInfoGenerator pass.

To enable SIL debugging and profiling for the Swift standard library, use
the build-script-impl option ``--build-sil-debugging-stdlib``.

Other Utilities
```````````````

To view the CFG of a function (or code region) in a SIL file, you can use the
script ``swift/utils/viewcfg``. It also works for LLVM IR files.
The script reads the SIL (or LLVM IR) code from stdin and displays the dot
graph file. Note: .dot files should be associated with the Graphviz app.

Using Breakpoints
`````````````````

LLDB has very powerful breakpoints, which can be utilized in many ways to debug
the compiler and Swift executables. The examples in this section show the LLDB
command lines. In Xcode you can set the breakpoint properties by clicking 'Edit
breakpoint'.

Let's start with a simple example: sometimes you see a function in the SIL
output and you want to know where the function was created in the compiler.
In this case you can set a conditional breakpoint in SILFunction constructor
and check for the function name in the breakpoint condition::

    (lldb) br set -c 'hasName("_TFC3nix1Xd")' -f SILFunction.cpp -l 91

Sometimes you may want to know which optimization inserts, removes or moves a
certain instruction. To find out, set a breakpoint in
``ilist_traits<SILInstruction>::addNodeToList`` or
``ilist_traits<SILInstruction>::removeNodeFromList``, which are defined in
``SILInstruction.cpp``.
The following command sets a breakpoint which stops if a ``strong_retain``
instruction is removed::

    (lldb) br set -c 'I->getKind() == ValueKind::StrongRetainInst' -f SILInstruction.cpp -l 63

The condition can be made more precise e.g. by also testing in which function
this happens::

    (lldb) br set -c 'I->getKind() == ValueKind::StrongRetainInst &&
               I->getFunction()->hasName("_TFC3nix1Xd")'
               -f SILInstruction.cpp -l 63

Let's assume the breakpoint hits somewhere in the middle of compiling a large
file. This is the point where the problem appears. But often you want to break
a little bit earlier, e.g. at the entrance of the optimization's ``run``
function.

To achieve this, set another breakpoint and add breakpoint commands::

    (lldb) br set -n GlobalARCOpts::run
    Breakpoint 2
    (lldb) br com add 2
    > p int $n = $n + 1
    > c
    > DONE

Run the program (this can take quite a bit longer than before). When the first
breakpoint hits see what value $n has::

    (lldb) p $n
    (int) $n = 5

Now remove the breakpoint commands from the second breakpoint (or create a new
one) and set the ignore count to $n minus one::

    (lldb) br delete 2
    (lldb) br set -i 4 -n GlobalARCOpts::run

Run your program again and the breakpoint hits just before the first breakpoint.

Another method for accomplishing the same task is to set the ignore count of the
breakpoint to a large number, i.e.::

    (lldb) br set -i 9999999 -n GlobalARCOpts::run

Then whenever the debugger stops next time (due to hitting another
breakpoint/crash/assert) you can list the current breakpoints::

    (lldb) br list
    1: name = 'GlobalARCOpts::run', locations = 1, resolved = 1, hit count = 85 Options: ignore: 1 enabled

which will then show you the number of times that each breakpoint was hit. In
this case, we know that ``GlobalARCOpts::run`` was hit 85 times. So, now
we know to ignore swift_getGenericMetadata 84 times, i.e.::

    (lldb) br set -i 84 -n GlobalARCOpts::run

LLDB Scripts
````````````

LLDB has powerful capabilities of scripting in Python among other languages. An
often overlooked, but very useful technique is the -s command to lldb. This
essentially acts as a pseudo-stdin of commands that lldb will read commands
from. Each time lldb hits a stopping point (i.e. a breakpoint or a
crash/assert), it will run the earliest command that has not been run yet. As an
example of this consider the following script (which without any loss of
generality will be called test.lldb)::

    env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib
    break set -n swift_getGenericMetadata
    break mod 1 -i 83
    process launch -- --stdlib-unittest-in-process --stdlib-unittest-filter "DefaultedForwardMutableCollection<OpaqueValue<Int>>.Type.subscript(_: Range)/Set/semantics"
    break set -l 224
    c
    expr pattern->CreateFunction
    break set -a $0
    c
    dis -f

TODO: Change this example to apply to the Swift compiler instead of to the
stdlib unittests.

Then by running ``lldb test -s test.lldb``, lldb will:

1. Enable guard malloc.
2. Set a break point on swift_getGenericMetadata and set it to be ignored for 83 hits.
3. Launch the application and stop at swift_getGenericMetadata after 83 hits have been ignored.
4. In the same file as swift_getGenericMetadata introduce a new breakpoint at line 224 and continue.
5. When we break at line 224 in that file, evaluate an expression pointer.
6. Set a breakpoint at the address of the expression pointer and continue.
7. When we hit the breakpoint set at the function pointer's address, disassemble
   the function that the function pointer was passed to.

Using LLDB scripts can enable one to use complex debugger workflows without
needing to retype the various commands perfectly every time.

Reducing SIL test cases using bug_reducer
`````````````````````````````````````````

There is functionality provided in ./swift/utils/bug_reducer/bug_reducer.py for
reducing SIL test cases by:

1. Producing intermediate sib files that only require some of the passes to
   trigger the crasher.
2. Reducing the size of the sil test case by extracting functions or
   partitioning a module into unoptimized and optimized modules.

For more information and a high level example, see:
./swift/utils/bug_reducer/README.md.


Debugging Swift Executables
---------------------------

One can use the previous tips for debugging the Swift compiler with Swift
executables as well. Here are some additional useful techniques that one can use
in Swift executables.

Determining the mangled name of a function in LLDB
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One problem that often comes up when debugging Swift code in LLDB is that LLDB
shows the demangled name instead of the mangled name. This can lead to mistakes
where due to the length of the mangled names one will look at the wrong
function. Using the following command, one can find the mangled name of the
function in the current frame::

    (lldb) image lookup -va $pc
    Address: CollectionType3[0x0000000100004db0] (CollectionType3.__TEXT.__text + 16000)
    Summary: CollectionType3`ext.CollectionType3.CollectionType3.MutableCollectionType2<A where A: CollectionType3.MutableCollectionType2>.(subscript.materializeForSet : (Swift.Range<A.Index>) -> Swift.MutableSlice<A>).(closure #1)
    Module: file = "/Volumes/Files/work/solon/build/build-swift/validation-test-macosx-x86_64/stdlib/Output/CollectionType.swift.gyb.tmp/CollectionType3", arch = "x86_64"
    Symbol: id = {0x0000008c}, range = [0x0000000100004db0-0x00000001000056f0), name="ext.CollectionType3.CollectionType3.MutableCollectionType2<A where A: CollectionType3.MutableCollectionType2>.(subscript.materializeForSet : (Swift.Range<A.Index>) -> Swift.MutableSlice<A>).(closure #1)", mangled="_TFFeRq_15CollectionType322MutableCollectionType2_S_S0_m9subscriptFGVs5Rangeqq_s16MutableIndexable5Index_GVs12MutableSliceq__U_FTBpRBBRQPS0_MS4__T_"
