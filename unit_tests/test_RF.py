import unittest
from pipeline import RegisterFile


class MyTestCase(unittest.TestCase):
    def setUp(self):
        self.RF = RegisterFile()

    def test_to_str(self):
        print(self.RF)
        self.assertIsNotNone(self.RF)

    def given_init_RF_when_test_in_ROB_then_false(self):
        for idx in range(self.RF.size):
            self.assertFalse(self.RF.in_ROB(idx))


if __name__ == '__main__':
    unittest.main()
