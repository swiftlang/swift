import unittest

from cmpcodesize.compare import listFunctionSizes


class ListFunctionSizesTestCase(unittest.TestCase):
    def test_when_size_array_is_none_raises(self):
        with self.assertRaises(TypeError):
            listFunctionSizes(None)

    def test_when_size_array_is_empty_returns_none(self):
        self.assertIsNone(listFunctionSizes([]))


if __name__ == '__main__':
    unittest.main()
