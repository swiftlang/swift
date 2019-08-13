libFuzzer Integration
---------------------

Swift compiler comes with a built-in ``libFuzzer`` integration.
In order to use it on a file ``myfile.swift``, we define an entry point fuzzing function
with a ``@_cdecl("LLVMFuzzerTestOneInput")`` annotation:


.. code-block:: swift

    @_cdecl("LLVMFuzzerTestOneInput") public func fuzzMe(Data: UnsafePointer<CChar>, Size: CInt) -> CInt{
        // Test our code using provided Data.
      }
    }

To compile it, we use ``-sanitize=fuzzer`` flag to link ``libFuzzer``
and enable coverage annotation, and ``-parse-as-library`` flag not to insert
the ``main`` symbol, such that the fuzzer entry point can be used:

.. code-block:: bash

    % swiftc -sanitize=fuzzer -parse-as-library myfile.swift

``libFuzzer`` can be also combined with other sanitizers:

.. code-block:: bash

    % swiftc -sanitize=fuzzer,address -parse-as-library myfile.swift

Finally, we launch the fuzzing process:

.. code-block:: bash

    % ./a.out

Refer to the official ``libFuzzer`` documentation at http://llvm.org/docs/LibFuzzer.html
for the description of flags the resulting binary has.
