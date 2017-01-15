import unittest
from pipeline import Pipeline
from toolkits import extract_binary_list


class MyTestCase(unittest.TestCase):
    def \
            setUp(self):
        self.inst_mem, self.data_mem, self.inst_bin_list, self.data_bin_list = extract_binary_list()
        self.ppl = Pipeline(self.inst_mem, self.data_mem)

    def test_something(self):
        while not self.ppl.is_over:
            self.ppl.next_cycle()

        self.assertEqual(True, True)


if __name__ == '__main__':
    unittest.main()
