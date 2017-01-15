import unittest
from pipeline import ReservationStation, InstructionQueue, BranchTargetBuffer
from toolkits import extract_binary_list


class MyTestCase(unittest.TestCase):
    def setUp(self):
        self.IQ = InstructionQueue()
        self.RS = ReservationStation()
        self.inst_mem, self.data_mem, self.inst_bin_list, self.data_bin_list = extract_binary_list()
        self.cycle = 0

    def test_to_str(self):
        for key in sorted(self.inst_mem.keys()):
            self.cycle += 1
            self.IQ.fetch(self.inst_mem[key], self.cycle)
            if not self.RS.avbl():
                break
            self.RS.add_entry(self.IQ.issue(self.cycle + 1))

        print(self.RS)
        self.assertIsNotNone(self.RS)


if __name__ == '__main__':
    unittest.main()
