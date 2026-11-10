# SIMD Vector Types

Work with fixed-width vectors of fixed-width numeric types of different sizes.

## Topics

### SIMD Vectors

- ``Swift/SIMD2``
- ``Swift/SIMD3``
- ``Swift/SIMD4``
- ``Swift/SIMD8``
- ``Swift/SIMD16``
- ``Swift/SIMD32``
- ``Swift/SIMD64``

### Supporting Types

- ``Swift/SIMD``
- ``Swift/SIMDScalar``
- ``Swift/SIMDStorage``
- ``Swift/SIMDMask``

### Supporting Functions

- ``Swift/all(_:)``
- ``Swift/all(_:)-(SIMDMask<SIMD32<Int64>>)`` <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD32<Int64>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD2<Int8>>)``   <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD2<Int8>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD2<Int>>)``    <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD2<Int>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD32<Int16>>)`` <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD32<Int16>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD16<Int32>>)`` <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD16<Int32>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD8<Int8>>)``   <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD8<Int8>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD3<Int16>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD3<Int16>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD3<Int32>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD3<Int32>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD32<Int32>>)`` <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD32<Int32>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD8<Int64>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD8<Int64>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD64<Int8>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD64<Int8>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD16<Int64>>)`` <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD16<Int64>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD4<Int64>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD4<Int64>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD3<Int>>)``    <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD3<Int>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD8<Int16>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD8<Int16>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD4<Int>>)``    <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD4<Int>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD4<Int8>>)``   <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD4<Int8>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD16<Int>>)``   <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD16<Int>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD2<Int16>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD2<Int16>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD64<Int64>>)`` <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD64<Int64>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD2<Int32>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD2<Int32>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD3<Int8>>)``   <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD3<Int8>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD8<Int32>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD8<Int32>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<Storage>)``       <!-- @export(implementation) func all<Storage>(_ mask: SIMDMask<Storage>) -> Bool where Storage : SIMD, Storage.Scalar : FixedWidthInteger, Storage.Scalar : SignedInteger -->
- ``Swift/all(_:)-(SIMDMask<SIMD32<Int>>)``   <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD32<Int>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD4<Int16>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD4<Int16>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD2<Int64>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD2<Int64>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD16<Int16>>)`` <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD16<Int16>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD3<Int64>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD3<Int64>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD32<Int8>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD32<Int8>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD4<Int32>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD4<Int32>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD64<Int32>>)`` <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD64<Int32>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD16<Int8>>)``  <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD16<Int8>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD8<Int>>)``    <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD8<Int>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD64<Int>>)``   <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD64<Int>>) -> Bool -->
- ``Swift/all(_:)-(SIMDMask<SIMD64<Int16>>)`` <!-- @export(implementation) func all(_ mask: SIMDMask<SIMD64<Int16>>) -> Bool -->
- ``Swift/any(_:)``
- ``/Swift/any(_:)-(SIMDMask<SIMD3<Int16>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD3<Int16>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD8<Int>>)``    <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD8<Int>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD3<Int>>)``    <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD3<Int>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD3<Int64>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD3<Int64>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD64<Int8>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD64<Int8>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD32<Int>>)``   <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD32<Int>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD16<Int16>>)`` <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD16<Int16>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD2<Int32>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD2<Int32>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD4<Int64>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD4<Int64>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD8<Int32>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD8<Int32>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD8<Int8>>)``   <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD8<Int8>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD32<Int32>>)`` <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD32<Int32>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD32<Int8>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD32<Int8>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD32<Int16>>)`` <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD32<Int16>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD64<Int16>>)`` <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD64<Int16>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD4<Int8>>)``   <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD4<Int8>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD2<Int8>>)``   <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD2<Int8>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<Storage>)``       <!-- @export(implementation) func any<Storage>(_ mask: SIMDMask<Storage>) -> Bool where Storage : SIMD, Storage.Scalar : FixedWidthInteger, Storage.Scalar : SignedInteger -->
- ``/Swift/any(_:)-(SIMDMask<SIMD16<Int8>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD16<Int8>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD2<Int64>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD2<Int64>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD2<Int16>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD2<Int16>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD3<Int32>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD3<Int32>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD8<Int16>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD8<Int16>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD4<Int32>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD4<Int32>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD2<Int>>)``    <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD2<Int>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD16<Int>>)``   <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD16<Int>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD4<Int16>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD4<Int16>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD8<Int64>>)``  <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD8<Int64>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD3<Int8>>)``   <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD3<Int8>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD16<Int32>>)`` <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD16<Int32>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD32<Int64>>)`` <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD32<Int64>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD4<Int>>)``    <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD4<Int>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD64<Int>>)``   <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD64<Int>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD64<Int64>>)`` <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD64<Int64>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD64<Int32>>)`` <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD64<Int32>>) -> Bool -->
- ``/Swift/any(_:)-(SIMDMask<SIMD16<Int64>>)`` <!-- @export(implementation) func any(_ mask: SIMDMask<SIMD16<Int64>>) -> Bool -->
- ``Swift/pointwiseMax(_:_:)-29hn2``
- ``Swift/pointwiseMax(_:_:)-2k6er``
- ``Swift/pointwiseMin(_:_:)-39txi``
- ``Swift/pointwiseMin(_:_:)-8v95p``
