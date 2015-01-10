Many of the tests in this directory are set up to model particular dependency graphs, which are set up via the folders in the Inputs/ directory. Most of these tests describe the dependency graph in ASCII art form at the top of the file.

a | b           Independent files 'a' and 'b'
a ==> b         File 'b' depends on file 'a'
a <==> b        File 'a' and file 'b' depend on each other
a --> b         File 'b' privately depends on file 'a' (normal dependencies cascade)
"./file" ==> a  File 'a' depends on external, non-source file './file'

Because of the way the tests are set up, the dependency information is put into the .swift files; any such test needs to start by "building" everything to copy that information into .swiftdeps files. To avoid timestamp issues, most of these tests start with:

    // RUN: rm -rf %t && cp -r %S/Inputs/<TEST_GRAPH>/ %t
    // RUN: touch -t 201401240005 %t/*


Some tests deal with the dependency graph being updated during the course of an incremental build. These tests have *two* ASCII dependency graphs at the top of the file, illustrating the "before" and "after" cases. This necessitates some additional artwork:

a +==> b  File 'a' changes its "provides" set in a way that affects file 'b'
a ==>+ b  File 'b' changes its "depends" set in a way that affects its dependency on file 'a'

In order to correctly run these tests, the "before" information is put into .swiftdeps files, while the "after" information is put into .swift files. The "after" information will be copied on top of the "before" information for any file that is "built". In order to not rebuild everything, each file also needs a dummy .o output file.

Most of these tests start with:

    // RUN: rm -rf %t && cp -r %S/Inputs/<TEST_GRAPH>/ %t
    // RUN: touch -t 201401240005 %t/*.swift
    // RUN: touch -t 201401240006 %t/*.o
