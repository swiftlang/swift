import unittest

from cmpcodesize.compare import listFunctionSizes


class ListFunctionSizesTestCase(unittest.TestCase):
    def test_when_size_array_is_none_raises(self):
        with self.assertRaises(TypeError):
            list(listFunctionSizes(None))

    def test_when_size_array_is_empty_returns_none(self):
        self.assertEqual(list(listFunctionSizes([])), [])

    def test_lists_each_entry(self):
        sizes = {
            'foo': 1,
            'bar': 10,
            'baz': 100,
        }
        self.assertEqual(list(listFunctionSizes(sizes.items())), [
            '       1 foo',
            '      10 bar',
            '     100 baz',
        ])

if __name__ == '__main__':
    unittest.main()
