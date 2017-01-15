import unittest
from pipeline import BranchTargetBuffer


class MyTestCase(unittest.TestCase):
    def setUp(self):
        self.BTB = BranchTargetBuffer()

    def given_full_BTB_when_add_then_delete_n_update(self):
        for idx in range(18):
            self.BTB.add_entry(600 + idx, 700 + idx, True)

        current_pc, _, _ = self.BTB.lookup_entry(601)
        self.assertIsNone(current_pc, 'early entry 601 should have been removed')
        current_pc, target_pc, taken = self.BTB.lookup_entry(617)
        self.assertEqual(717, target_pc, "target PC should be 717 for BTB entry 617")


if __name__ == '__main__':
    unittest.main()
