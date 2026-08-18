Explicit Code Blocks
====================

reStructuredText also supports explicit code blocks, using ``code-block`` and
``sourcecode`` block markup.

.. code-block::

   // CHECK: SIMPLE
   print("SIMPLE")

.. sourcecode::

   // CHECK: SOURCECODE
   print("SOURCECODE")

These can specify the language used; we only support ``swift``:

.. code-block:: swift

   // CHECK: SWIFT
   print("SWIFT")

.. code-block:: pascal

   // CHECK-NOT: PASCAL
   print("PASCAL")

We also support a ``nocompile`` option for these, with or without the ``swift``
language type:

.. code-block::
   :nocompile:

   // CHECK-NOT: NOCOMPILE
   print("NOCOMPILE")

.. code-block:: swift
   :nocompile:

   // CHECK-NOT: SWIFT-NOCOMPILE
   print("SWIFT-NOCOMPILE")

As with indented code blocks, they can contain indentation

.. code-block:: swift

   // CHECK: INDENTATION OK. SUM IS 55
   var sum = 0
   for n in 1...10 {
     sum += n
   }
   print("INDENTATION OK. SUM IS \(sum)")

as well as blank lines

.. code-block:: swift

   // CHECK: BLANK NO SPACES

   print("BLANK NO SPACES")

with or without indentation

.. code-block:: swift

   // CHECK: BLANK SPACES
   
   print("BLANK SPACES")
