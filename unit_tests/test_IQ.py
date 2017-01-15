import unittest
from pipeline import InstructionQueue, BranchTargetBuffer
from toolkits import extract_binary_list


class MyTestCase(unittest.TestCase):
    def setUp(self):
        self.IQ = InstructionQueue()
        self.inst_mem, self.data_mem, self.inst_bin_list, self.data_bin_list = extract_binary_list()
        self.cycle = 0

    def test_IQ_fetch_n_to_str(self):
        for key in sorted(self.inst_mem.keys()):
            self.cycle += 1
            self.IQ.fetch(self.inst_mem[key], self.cycle)

        print(self.IQ)
        self.assertIsNotNone(self.IQ)


if __name__ == '__main__':
    unittest.main()
