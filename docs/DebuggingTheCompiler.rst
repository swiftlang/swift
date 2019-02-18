:orphan:

.. highlight:: none

.. contents::

Debugging the Swift Compiler
============================

Abstract
--------

This document contains some useful information for debugging the
Swift compiler and Swift compiler output.

Basic Utilities
---------------

Often, the first step to debug a compiler problem is to re-run the compiler
with a command line, which comes from a crash trace or a build log.

The script ``split-cmdline`` in ``utils/dev-scripts`` splits a command line
into multiple lines. This is helpful to understand and edit long command lines.

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

Debugging the Type Checker
--------------------------

Enabling Logging
~~~~~~~~~~~~~~~~

To enable logging in the type checker, use the following argument: ``-Xfrontend -debug-constraints``.
This will cause the typechecker to log its internal state as it solves
constraints and present the final type checked solution, e.g.::

  ---Constraint solving for the expression at [test.swift:3:10 - line:3:10]---
  ---Initial constraints for the given expression---
  (integer_literal_expr type='$T0' location=test.swift:3:10 range=[test.swift:3:10 - line:3:10] value=0)
  Score: 0 0 0 0 0 0 0 0 0 0 0 0 0
  Contextual Type: Int
  Type Variables:
    #0 = $T0 [inout allowed]

  Active Constraints:

  Inactive Constraints:
    $T0 literal conforms to ExpressibleByIntegerLiteral [[locator@0x7ffa3a865a00 [IntegerLiteral@test.swift:3:10]]];
    $T0 conv Int [[locator@0x7ffa3a865a00 [IntegerLiteral@test.swift:3:10]]];
  ($T0 literal=3 bindings=(subtypes of) (default from ExpressibleByIntegerLiteral) Int)
  Active bindings: $T0 := Int
  (trying $T0 := Int
    (found solution 0 0 0 0 0 0 0 0 0 0 0 0 0)
  )
  ---Solution---
  Fixed score: 0 0 0 0 0 0 0 0 0 0 0 0 0
  Type variables:
    $T0 as Int

  Overload choices:

  Constraint restrictions:

  Disjunction choices:

  Conformances:
    At locator@0x7ffa3a865a00 [IntegerLiteral@test.swift:3:10]
  (normal_conformance type=Int protocol=ExpressibleByIntegerLiteral lazy
    (normal_conformance type=Int protocol=_ExpressibleByBuiltinIntegerLiteral lazy))
  (found solution 0 0 0 0 0 0 0 0 0 0 0 0 0)
  ---Type-checked expression---
  (call_expr implicit type='Int' location=test.swift:3:10 range=[test.swift:3:10 - line:3:10] arg_labels=_builtinIntegerLiteral:
    (constructor_ref_call_expr implicit type='(_MaxBuiltinIntegerType) -> Int' location=test.swift:3:10 range=[test.swift:3:10 - line:3:10]
      (declref_expr implicit type='(Int.Type) -> (_MaxBuiltinIntegerType) -> Int' location=test.swift:3:10 range=[test.swift:3:10 - line:3:10] decl=Swift.(file).Int.init(_builtinIntegerLiteral:) function_ref=single)
      (type_expr implicit type='Int.Type' location=test.swift:3:10 range=[test.swift:3:10 - line:3:10] typerepr='Int'))
    (tuple_expr implicit type='(_builtinIntegerLiteral: Int2048)' location=test.swift:3:10 range=[test.swift:3:10 - line:3:10] names=_builtinIntegerLiteral
      (integer_literal_expr type='Int2048' location=test.swift:3:10 range=[test.swift:3:10 - line:3:10] value=0)))

When using the integrated swift-repl, one can dump the same output for each
expression as one evaluates the expression by enabling constraints debugging by
typing ``:constraints debug on``::

  $ swift -frontend -repl -enable-objc-interop -module-name REPL
  ***  You are running Swift's integrated REPL,  ***
  ***  intended for compiler and stdlib          ***
  ***  development and testing purposes only.    ***
  ***  The full REPL is built as part of LLDB.   ***
  ***  Type ':help' for assistance.              ***
  (swift) :constraints debug on

Asserting on First Error
~~~~~~~~~~~~~~~~~~~~~~~~

When changing the typechecker, one can cause a series of cascading errors. Since
Swift doesn't assert on such errors, one has to know more about the typechecker
to know where to stop in the debugger. Rather than doing that, one can use the
option ``-Xllvm -swift-diagnostics-assert-on-error=1`` to cause the
DiagnosticsEngine to assert upon the first error, providing the signal that the
debugger needs to know that it should attach.

Debugging on SIL Level
----------------------

Options for Dumping the SIL
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The compiler provides a way to debug and profile on SIL level. To enable SIL
debugging add the front-end option -gsil together with -g. Example::

    swiftc -g -Xfrontend -gsil -O test.swift -o a.out

This writes the SIL after optimizations into a file and generates debug info
for it. In the debugger and profiler you can then see the SIL code instead of
the Swift source code.
For details see the SILDebugInfoGenerator pass.

To enable SIL debugging and profiling for the Swift standard library, use
the build-script-impl option ``--build-sil-debugging-stdlib``.

ViewCFG: Regex based CFG Printer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ViewCFG (``./utils/viewcfg``) is a script that parses a textual CFG (e.g. a llvm
or sil function) and displays a .dot file of the CFG. Since the parsing is done
using regular expressions (i.e. ignoring language semantics), ViewCFG can:

1. Parse both SIL and LLVM IR
2. Parse blocks and functions without needing to know contextual
   information. Ex: types and declarations.

The script assumes that the relevant text is passed in via stdin and uses open
to display the .dot file.

Additional, both emacs and vim integration is provided. For vim integration add
the following commands to your .vimrc::

  com! -nargs=? Funccfg silent ?{$?,/^}/w !viewcfg <args>
  com! -range -nargs=? Viewcfg silent <line1>,<line2>w !viewcfg <args>

This will add::

   :Funccfg        displays the CFG of the current SIL/LLVM function.
   :<range>Viewcfg displays the sub-CFG of the selected range.

For emacs users, we provide in sil-mode (``./utils/sil-mode.el``) the function::

    sil-mode-display-function-cfg

To use this feature, placed the point in the sil function that you want viewcfg
to graph and then run ``sil-mode-display-function-cfg``. This will cause viewcfg
to be invoked with the sil function body. Note,
``sil-mode-display-function-cfg`` does not take any arguments.

**NOTE** viewcfg must be in the $PATH for viewcfg to work.

**NOTE** Since we use open, .dot files should be associated with the Graphviz app for viewcfg to work.

There is another useful script to view the CFG of a disassembled function:
``./utils/dev-scripts/blockifyasm``.
It splits a disassembled function up into basic blocks which can then be
used with viewcfg::

    (lldb) disassemble
      <copy-paste output to file.s>
    $ blockifyasm < file.s | viewcfg

Using Breakpoints
~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~

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

Custom LLDB Commands
~~~~~~~~~~~~~~~~~~~~

If you've ever found yourself repeatedly entering a complex sequence of
commands within a debug session, consider using custom lldb commands. Custom
commands are a handy way to automate debugging tasks.

For example, say we need a command that prints the contents of the register
``rax`` and then steps to the next instruction. Here's how to define that
command within a debug session::

    (lldb) script
    Python Interactive Interpreter. To exit, type 'quit()', 'exit()' or Ctrl-D.
    >>> def custom_step():
    ...   print "rax =", lldb.frame.FindRegister("rax")
    ...   lldb.thread.StepInstruction(True)
    ...
    >>> ^D

You can call this function using the ``script`` command, or via an alias::

    (lldb) script custom_step()
    rax = ...
    <debugger steps to the next instruction>

    (lldb) command alias cs script custom_step()
    (lldb) cs
    rax = ...
    <debugger steps to the next instruction>

Printing registers and single-stepping are by no means the only things you can
do with custom commands. The LLDB Python API surfaces a lot of useful
functionality, such as arbitrary expression evaluation.

There are some pre-defined custom commands which can be especially useful while
debugging the swift compiler. These commands live in
``swift/utils/lldb/lldbToolBox.py``. There is a wrapper script available in
``SWIFT_BINARY_DIR/bin/lldb-with-tools`` which launches lldb with those
commands loaded.

A command named ``sequence`` is included in lldbToolBox. ``sequence`` runs
multiple semicolon separated commands together as one command. This can be used
to define custom commands using just other lldb commands. For example,
``custom_step()`` function defined above could be defined as::

    (lldb) command alias cs sequence p/x $rax; stepi

Reducing SIL test cases using bug_reducer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There is functionality provided in ./swift/utils/bug_reducer/bug_reducer.py for
reducing SIL test cases by:

1. Producing intermediate sib files that only require some of the passes to
   trigger the crasher.
2. Reducing the size of the sil test case by extracting functions or
   partitioning a module into unoptimized and optimized modules.

For more information and a high level example, see:
./swift/utils/bug_reducer/README.md.

Using ``clang-tidy`` to run the Static Analyzer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Recent versions of LLVM package the tool ``clang-tidy``. This can be used in
combination with a json compilation database to run static analyzer checks as
well as cleanups/modernizations on a code-base. Swift's cmake invocation by
default creates one of these json databases at the root path of the swift host
build, for example on macOS::

    $PATH_TO_BUILD/swift-macosx-x86_64/compile_commands.json

Using this file, one invokes ``clang-tidy`` on a specific file in the codebase
as follows::

    clang-tidy -p=$PATH_TO_BUILD/swift-macosx-x86_64/compile_commands.json $FULL_PATH_TO_FILE

One can also use shell regex to visit multiple files in the same directory. Example::

    clang-tidy -p=$PATH_TO_BUILD/swift-macosx-x86_64/compile_commands.json $FULL_PATH_TO_DIR/*.cpp

Debugging Swift Executables
===========================

One can use the previous tips for debugging the Swift compiler with Swift
executables as well. Here are some additional useful techniques that one can use
in Swift executables.

Determining the mangled name of a function in LLDB
--------------------------------------------------

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

Manually symbolication using LLDB
---------------------------------

One can perform manual symbolication of a crash log or an executable using LLDB
without running the actual executable. For a detailed guide on how to do this,
see: https://lldb.llvm.org/symbolication.html.

Debugging LLDB failures
=======================

Sometimes one needs to be able to while debugging actually debug LLDB and its
interaction with Swift itself. Some examples of problems where this can come up
are:

1. Compiler bugs when LLDB attempts to evaluate an expression. (expression
   debugging)
2. Swift variables being shown with no types. (type debugging)

To gain further insight into these sorts of failures, we use LLDB log
categories. LLDB log categories provide introspection by causing LLDB to dump
verbose information relevant to the category into the log as it works. The two
log channels that are useful for debugging Swift issues are the "types" and
"expression" log channels.

For more details about any of the information below, please run::

    (lldb) help log enable

"Types" Log
-----------

The "types" log reports on LLDB's process of constructing SwiftASTContexts and
errors that may occur. The two main tasks here are:

1. Constructing the SwiftASTContext for a specific single Swift module. This is
   used to implement frame local variable dumping via the lldb ``frame
   variable`` command, as well as the Xcode locals view. On failure, local
   variables will not have types.

2. Building a SwiftASTContext in which to run Swift expressions using the
   "expression" command. Upon failure, one will see an error like: "Shared Swift
   state for has developed fatal errors and is being discarded."

These errors can be debugged by turning on the types log::

    (lldb) log enable -f /tmp/lldb-types-log.txt lldb types

That will write the types log to the file passed to the -f option.

**NOTE** Module loading can happen as a side-effect of other operations in lldb
 (e.g. the "file" command). To be sure that one has enabled logging before /any/
 module loading has occurred, place the command into either::

   ~/.lldbinit
   $PWD/.lldbinit

This will ensure that the type import command is run before /any/ modules are
imported.

"Expression" Log
----------------

The "expression" log reports on the process of wrapping, parsing, SILGen'ing,
JITing, and inserting an expression into the current Swift module. Since this can
only be triggered by the user manually evaluating expression, this can be turned
on at any point before evaluating an expression. To enable expression logging,
first run::

    (lldb) log enable -f /tmp/lldb-expr-log.txt lldb expression

and then evaluate the expression. The expression log dumps, in order, the
following non-exhaustive list of state:

1. The unparsed, textual expression passed to the compiler.
2. The parsed expression.
3. The initial SILGen.
4. SILGen after SILLinking has occurred.
5. SILGen after SILLinking and Guaranteed Optimizations have occurred.
6. The resulting LLVM IR.
7. The assembly code that will be used by the JIT.

**NOTE** LLDB runs a handful of preparatory expressions that it uses to set up
for running Swift expressions. These can make the expression logs hard to read
especially if one evaluates multiple expressions with the logging enabled. In
such a situation, run all expressions before the bad expression, turn on the
logging, and only then run the bad expression.

Multiple Logs at a Time
-----------------------

Note, you can also turn on more than one log at a time as well, e.x.::

    (lldb) log enable -f /tmp/lldb-types-log.txt lldb types expression
