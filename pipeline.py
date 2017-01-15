from mips32 import Instruction, InstructionJump, InstructionBranchOnEqual, InstructionBranchOnGreaterThanOrEqualToZero, \
    InstructionBranchOnGreaterThanZero, InstructionBranchOnLessThanOrEqualToZero, InstructionBranchOnLessThanZero, \
    InstructionBranchOnNotEqual, Data, InstructionStoreWord, InstructionLoadWord, InstructionAddImmediateWord, \
    InstructionAddWord, InstructionNoOperation, InstructionBreakpoint
from collections import OrderedDict
from typing import Dict
from enum import Enum


class _InstTypes(Enum):
    SL = 1
    ALU = 2
    BRCH = 3
    UKNW = 4


class _PipelineState(Enum):
    FETCH = 1
    ISSUE = 2
    EXEC = 3
    MEMACCESS = 6
    WB = 4
    COMMIT = 5


class _PipelineInstEntry:
    def __init__(self, inst: Instruction, prd_taken=False):
        self.inst = inst
        self.pc_val = inst.pc_val
        self.fetch_cycle = -1
        self.issue_cycle = -1
        self.exec_cycle = -1
        self.wb_cycle = -1
        self.commit_cycle = -1
        self.is_issued = False
        self.is_wbed = False
        self.is_execed = False
        self.is_committed = False
        self.prd_taken = prd_taken
        self.mis_predict = False

    def issue(self, cycle):
        """
        when the instruction being issued, decoding should happened
        :param cycle:
        :return:
        """
        self.issue_cycle = cycle
        self.is_issued = True

    def get_type(self) -> _InstTypes:
        branch_inst_set = (InstructionJump, InstructionBranchOnEqual, InstructionBranchOnGreaterThanOrEqualToZero,
                           InstructionBranchOnGreaterThanZero, InstructionBranchOnLessThanOrEqualToZero,
                           InstructionBranchOnLessThanZero,
                           InstructionBranchOnNotEqual)
        sl_inst_set = (InstructionStoreWord, InstructionLoadWord)
        alu_inst_set = (InstructionAddImmediateWord, InstructionAddWord)

        if isinstance(self.inst, alu_inst_set):
            inst_type = _InstTypes.ALU
        elif isinstance(self.inst, sl_inst_set):
            inst_type = _InstTypes.SL
        elif isinstance(self.inst, branch_inst_set):
            inst_type = _InstTypes.BRCH
        else:
            inst_type = _InstTypes.UKNW
        return inst_type


class _BTBEntry:
    def __init__(self, current_pc, target_pc, taken=False):
        self.current_pc = current_pc
        self.target_pc = target_pc
        self.taken = taken


class _RSEntry:
    def __init__(self, pip_inst: _PipelineInstEntry, v_j: int = None, v_k: int = None, q_j: int = None,
                 q_k: int = None):
        self.pip_inst = pip_inst
        self.rob_idx = pip_inst.issue_cycle
        self.v_j = v_j
        self.v_k = v_k
        self.q_j = q_j
        self.q_k = q_k
        self.exec_locking = False

    def is_ready_for_exec(self) -> bool:
        is_ready = False
        if self.q_j is None and self.q_k is None and not self.exec_locking:
            # if self.q_j is None and self.q_k is None:
            is_ready = True
        return is_ready


class _RegisterAllocationUnit:
    def __init__(self):
        self.in_rob = False
        self.rob_idx = -1

    def reset(self):
        self.in_rob = False
        self.rob_idx = -1


class _ROBEntry:
    def __init__(self, pip_inst: _PipelineInstEntry):
        self.pip_inst = pip_inst
        self.issue_id = pip_inst.issue_cycle
        # action: ALU, store/load, branch, other(NOP, BREAK)
        # destination should be the target Register index
        self.dest = pip_inst.inst.dest
        self.status = _PipelineState.ISSUE  # indicate current state (exec, wb or commit)
        self.value = None
        self.ready_commit = False
        self.dest_in_rob = False


class Pipeline:
    pc = 600
    cycle = 0
    inst_size = 4

    def __init__(self, inst_mem: Dict[int, Instruction], data_mem: Dict[int, Data]):
        self.cbd = {}
        self.BTB = BranchTargetBuffer()
        self.IQ = InstructionQueue()
        self.RF = RegisterFile()
        self.RS = ReservationStation(self.RF, self.cbd)
        self.inst_mem = inst_mem
        self.DS = DataSegment(data_mem)
        self.ROB = ReorderBuffer(self.RF, self.DS)
        self.next_pc = self.pc
        self.is_over = False

    def next_cycle(self):
        self.cycle += 1
        print('\nCycle <{}>:'.format(self.cycle))  # print the current cycle number

        # if there's still inst in the mem, fetch it
        next_inst_to_fetch = self.inst_mem.get(self.pc, None)
        if next_inst_to_fetch is not None:
            self.fetch(next_inst_to_fetch)
        # handle not to fetch and issue at same cycle
        # to issue an Instruction, both RS and ROB should be available
        pip_inst = self.IQ.next_issue_inst()
        # Stall situation(either RS or ROB is full) will be handled before issue the instruction
        if pip_inst is not None and pip_inst.fetch_cycle != self.cycle and self.RS.avbl() and self.ROB.avbl():
            self.issue()

        # if there's inst(First) ready for execution, execute
        while True:
            rs_inst_entry = self.RS.next_exec_entry()
            if rs_inst_entry is not None and rs_inst_entry.pip_inst.issue_cycle != self.cycle:
                self.execute()
            else:
                break

        # if there's insts ready for wb, wb.
        while True:
            rob_inst_entry = self.ROB.next_wb_entry(self.cycle)
            if rob_inst_entry is not None and rob_inst_entry.pip_inst.exec_cycle != self.cycle:
                self.write_back()
            else:
                break

        # # if next is mem access, can be wb in same cycle
        # rob_inst_entry = self.ROB.next_wb_entry(self.cycle)
        # if rob_inst_entry is not None and rob_inst_entry.pip_inst.exec_cycle != self.cycle \
        #         and rob_inst_entry.status == _PipelineState.MEMACCESS:
        #     self.write_back()

        # if there's inst(First) ready for commit, commit
        rob_inst_entry = self.ROB.next_commit_entry()
        if rob_inst_entry is not None and rob_inst_entry.pip_inst.wb_cycle != self.cycle and rob_inst_entry.pip_inst.issue_cycle != self.cycle:
            self.commit()

        # self.snapshot()
        self.pc = self.next_pc

    def fetch(self, inst: Instruction):
        _, target_pc, prd_taken = self.BTB.lookup_entry(inst.pc_val)
        # check branch prediction
        if prd_taken:
            # branch target found, jump to target
            self.next_pc = target_pc
        else:
            self.next_pc += self.inst_size

        self.IQ.fetch(inst, self.cycle, prd_taken)

    def issue(self):
        # issue out one instruction from IQ
        pip_inst = self.IQ.issue(self.cycle)
        # issue to RS
        if not isinstance(pip_inst.inst, (InstructionNoOperation, InstructionBreakpoint)):
            self.RS.add_entry(pip_inst)
        # issue to ROB
        self.ROB.add_entry(pip_inst)
        # only ALU will write to Register
        # register ID in the Register File
        if pip_inst.get_type() == _InstTypes.ALU or isinstance(pip_inst.inst, InstructionLoadWord):
            self.RF.associate_register(pip_inst.inst.dest, self.cycle)

    def execute(self):
        rs_entry = self.RS.next_exec_entry()
        print('executing:' + rs_entry.pip_inst.inst.desc_str)
        rs_entry.pip_inst.exec_cycle = self.cycle
        rs_entry.exec_locking = True
        # push the entry to ALU unit/SL operations. if more than one isnt is ready for dispatch, dispatch the older one
        pip_inst = rs_entry.pip_inst

        if pip_inst.get_type() == _InstTypes.ALU:
            result = rs_entry.v_j + rs_entry.v_k
            # update exec result to ROB
            self.ROB.execution(rs_entry.rob_idx, result)
        elif pip_inst.get_type() == _InstTypes.SL:
            if isinstance(pip_inst.inst, InstructionStoreWord):
                mem_addr = rs_entry.v_j + rs_entry.v_k
                self.ROB.execution(rs_entry.rob_idx, mem_addr)
            elif isinstance(pip_inst.inst, InstructionLoadWord):
                mem_addr = rs_entry.v_j + rs_entry.v_k
                # update exec result to ROB
                self.ROB.execution(rs_entry.rob_idx, mem_addr)
        elif pip_inst.get_type() == _InstTypes.BRCH:
            value = 0
            src_pc, target_pc, prd_taken = self.BTB.lookup_entry(pip_inst.inst.pc_val)

            if isinstance(pip_inst.inst, InstructionBranchOnEqual):
                brch_taken = (rs_entry.v_j == rs_entry.v_k)
                value = pip_inst.inst.pc_val + self.inst_size + pip_inst.inst.dest
                if src_pc is None:
                    # branch not in BTB, insert into BTB
                    self.BTB.add_entry(pip_inst.inst.pc_val, value, False)
            elif isinstance(pip_inst.inst, InstructionJump):
                brch_taken = True
                value = pip_inst.inst.dest

            # check branch prediction
            pip_inst.mis_predict = (brch_taken != prd_taken)

            # BRCH mis predict(except jump) will be updated
            if pip_inst.mis_predict and isinstance(pip_inst.inst, InstructionBranchOnEqual):
                self.BTB.update_mispredict(pip_inst.inst.pc_val)

            self.ROB.execution(rs_entry.rob_idx, value)

    def write_back(self):
        rob_entry = self.ROB.next_wb_entry(self.cycle)
        print('wb:' + rob_entry.pip_inst.inst.desc_str)
        rob_entry.pip_inst.wb_cycle = self.cycle
        if rob_entry.status == _PipelineState.MEMACCESS:
            rob_entry.pip_inst.exec_cycle = self.cycle
        else:
            # broadcast info in RS
            if isinstance(rob_entry.pip_inst.inst, InstructionLoadWord):
                self.RS.receive_wb(rob_entry.issue_id, self.DS.mem_read(rob_entry.value))
                self.cbd[rob_entry.issue_id] = self.DS.mem_read(rob_entry.value)
            else:
                self.RS.receive_wb(rob_entry.issue_id, rob_entry.value)
                self.cbd[rob_entry.issue_id] = rob_entry.value
        self.ROB.wb(rob_entry.issue_id, rob_entry.value)

    def commit(self):
        rob_entry = self.ROB.next_commit_entry()
        print('commit:' + rob_entry.pip_inst.inst.desc_str)
        pip_inst = rob_entry.pip_inst
        inst_type = pip_inst.get_type()

        # beore commit, still need to broadcast incase dangling reference
        self.ROB.wb(rob_entry.issue_id, rob_entry.value)
        self.RS.receive_wb(rob_entry.issue_id, rob_entry.value)

        if inst_type == _InstTypes.ALU:
            # write value to register
            self.RF.commit_in_value(rob_entry.dest, rob_entry.value, rob_entry.issue_id)
        elif inst_type == _InstTypes.SL:
            # commit to mem
            if isinstance(rob_entry.pip_inst.inst, InstructionStoreWord):
                # Memory store
                self.DS.mem_write(rob_entry.value, rob_entry.dest)
            else:
                # Memory Load
                readout_val = self.DS.mem_read(rob_entry.value)
                # update the register
                self.RF.commit_in_value(rob_entry.dest, readout_val, rob_entry.issue_id)
        elif inst_type == _InstTypes.BRCH:
            if pip_inst.mis_predict:
                # flush in IQ after current inst fetch cycle
                self.IQ.flush_from(pip_inst.fetch_cycle)
                # flush in ROB after current inst issue cycle(rob id)
                self.ROB.reset()
                # flush in RS after current inst issue cycle(rob id)
                self.RS.flush_from(pip_inst.issue_cycle)
                # dis associate instruction in RF which associated with current inst issue cycle(rob id)
                if pip_inst.prd_taken:
                    self.next_pc = pip_inst.inst.pc_val + self.inst_size
                else:
                    self.next_pc = rob_entry.value

            src_pc, target_pc, prd_taken = self.BTB.lookup_entry(pip_inst.inst.pc_val)
            if isinstance(pip_inst.inst, InstructionJump) and src_pc is None:
                self.BTB.add_entry(pip_inst.inst.pc_val, pip_inst.inst.dest, True)
            else:
                if pip_inst.mis_predict and isinstance(pip_inst.inst, InstructionJump):
                    self.BTB.update_mispredict(pip_inst.inst.pc_val)

        else:
            # print('Unsupported instruction?')
            pass

        # delete instruction from RS and ROB
        self.ROB.pop_entry(rob_entry.issue_id)
        self.RS.pop_entry(rob_entry.issue_id)

        if isinstance(rob_entry.pip_inst.inst, InstructionBreakpoint):
            self.is_over = True

    def snapshot(self):
        # print('Cycle <{}>:'.format(self.cycle))  # print the current cycle number
        print(self.IQ, end="")  # print IQ
        print(self.RS, end="")  # print RS
        print(self.ROB, end="")  # print ROB
        print(self.BTB, end="")  # print BTB
        print(self.RF, end="")  # print RF
        print(self.DS, end="")  # print Data Segment


class InstructionQueue:
    """
    unlimited size. Two things todo: 1. add inst to IQ, 2. check/predict branch
    """

    def __init__(self):
        self.queue = []
        self.next_issue_idx = 0

    def q_size(self):
        return len(self.queue)

    def fetch(self, inst: Instruction, cycle: int, prd_taken: bool):
        pip_inst = _PipelineInstEntry(inst, prd_taken)
        pip_inst.fetch_cycle = cycle
        self.queue.append(pip_inst)

    def flush_from(self, flush_point: int):
        for idx, pip_inst in enumerate(list(self.queue)):
            if pip_inst.fetch_cycle > flush_point:
                self.queue.pop(-1)

        self.next_issue_idx = len(self.queue)

    def next_issue_inst(self) -> _PipelineInstEntry:
        inst = None
        if len(self.queue) > 0 and self.next_issue_idx < len(self.queue):
            inst = self.queue[self.next_issue_idx]
        return inst

    def issue(self, cycle: int) -> _PipelineInstEntry:
        """
        The pipeline instruction issued OUT from the pipeline
        :param cycle:
        :return:
        """
        if len(self.queue) > 0:
            pip_inst = self.next_issue_inst()  # get the next instruction to be issued
            pip_inst.issue(cycle)  # update the issue status in the entry
            self.next_issue_idx += 1
        else:
            pip_inst = None
        return pip_inst

    def __str__(self):

        desc_str = 'IQ:\n'
        for pip_inst in self.queue:
            if pip_inst.is_issued:
                continue
            desc_str += '[{}]\n'.format(pip_inst.inst.desc_str)
        # end for
        return desc_str


class BranchTargetBuffer:
    """
    16 entries, fully-associative with LRU replacement, based on the PC address.
    Each entry of BTB records the corresponding PC address, the target PC, and a 1-bit predictor.
    """

    def __init__(self):
        self.size = 16
        self._table = OrderedDict()

    def __str__(self):
        desc_str = 'BTB:\n'
        for idx, (cycle, btb_entry) in enumerate(self._table.items()):
            desc_str += '[Entry {}]<{},{},{}>\n'.format(idx + 1, btb_entry.current_pc, btb_entry.target_pc,
                                                        1 if btb_entry.taken else 0)
        # end for
        return desc_str

    def add_entry(self, current_pc: int, target_pc: int, taken=True):
        if len(self._table) >= self.size:
            # if the BTB is full pop the earliest entry and add the new one
            self._table.popitem(last=False)
        self._table[current_pc] = _BTBEntry(current_pc, target_pc, taken)

    def update_mispredict(self, current_pc: int):
        entry = self._table.get(current_pc)
        entry.taken = not entry.taken

    def update_entry(self, current_pc: int, target_pc: int, taken=True):
        _, target, _ = self.lookup_entry(current_pc)
        if target is None:
            # not exists, insert
            self.add_entry(current_pc, target_pc, taken)
        else:
            self._table[current_pc].taken = taken

    def list_entries(self):
        return self._table.keys()

    def lookup_entry(self, pc_val: int) -> (int, int, bool):
        entry = self._table.get(pc_val)

        if entry is None:
            return None, None, False
        else:
            return entry.current_pc, entry.target_pc, entry.taken


class RegisterFile:
    """
    32 integer registers. unlimited R/W ports, no hardware hazard for register read/write.
    """

    def __init__(self):
        self.size = 32
        self.print_width = 8
        self.R = [0] * self.size
        self._table_RAT = []
        for idx in range(self.size):
            self._table_RAT.append(_RegisterAllocationUnit())

    def __str__(self):
        desc_str = 'Registers:'
        for idx in range(len(self.R)):
            if idx % self.print_width == 0:
                desc_str += '\nR{}:'.format(str(idx).zfill(2))
            # end if
            desc_str += '\t' + str(self.R[idx])
        # end for
        desc_str += '\n'
        return desc_str

    def flush_from(self, start_point: int):
        for entry in self._table_RAT:
            if entry.rob_idx >= start_point:
                entry.reset()

    def associate_register(self, reg_idx, rob_idx):
        self._table_RAT[reg_idx].rob_idx = rob_idx
        self._table_RAT[reg_idx].in_rob = True

    def load_register(self, reg_idx: int) -> (bool, int, int):
        rob_idx = self._table_RAT[reg_idx].rob_idx
        in_rob = self._table_RAT[reg_idx].in_rob
        if in_rob:
            reg_val = self.R[reg_idx]
        else:
            reg_val = self.R[reg_idx]
        return in_rob, rob_idx, reg_val

    def in_ROB(self, reg_idx) -> bool:
        return self._table_RAT[reg_idx].in_rob

    def commit_in_value(self, reg_idx, value, rob_idx):
        self.R[reg_idx] = value
        if self._table_RAT[reg_idx].rob_idx == rob_idx:
            self._table_RAT[reg_idx].reset()


class ReservationStation:
    """
    10 integer ALU RS entries (for 10 integer units), for ALU instructions, address generation for load and store
    instructions, and for branch and jump.
    """

    def __init__(self, RF: RegisterFile, cbd: Dict[int, int]):
        self.size = 10
        self.queue = []
        self.ref_RF = RF
        self.ref_cb = cbd

    def avbl(self):
        avbl = False
        if len(self.queue) < self.size:
            avbl = True
        # end if
        return avbl

    def __str__(self):
        desc_str = 'RS:\n'

        for rs_entry in self.queue:
            desc_str += '[{}]\n'.format(rs_entry.pip_inst.inst.desc_str)
        # end for
        return desc_str

    def flush_from(self, start_point: int):
        for idx, entry in enumerate(list(self.queue)):
            if entry.pip_inst.issue_cycle > start_point:
                self.queue.pop(-1)

    def add_entry(self, pip_inst: _PipelineInstEntry):
        success = False
        if self.avbl():
            vj, vk, qj, qk = self.decode(pip_inst.inst)
            self.queue.append(_RSEntry(pip_inst, vj, vk, qj, qk))
            success = True
        # end if
        return success

    def decode(self, inst: Instruction) -> (int, int, _RegisterAllocationUnit, _RegisterAllocationUnit):
        op1 = inst.op1_val
        op2 = inst.op2_val
        vj = None
        vk = None
        qj = None
        qk = None
        if isinstance(inst, InstructionAddImmediateWord):
            vk = op2
            in_rob, rob_idx, reg_val = self.ref_RF.load_register(op1)
            if in_rob:
                val = self.ref_cb.get(rob_idx)
                if val is None:
                    qj = rob_idx
                else:
                    vj = val
            else:
                vj = reg_val
        elif isinstance(inst, InstructionAddWord):
            in_rob, rob_idx, reg_val = self.ref_RF.load_register(op1)
            if in_rob:
                val = self.ref_cb.get(rob_idx)
                if val is None:
                    qj = rob_idx
                else:
                    vj = val
            else:
                vj = reg_val
            in_rob, rob_idx, reg_val = self.ref_RF.load_register(op2)
            if in_rob:
                val = self.ref_cb.get(rob_idx)
                if val is None:
                    qk = rob_idx
                else:
                    vk = val
            else:
                vk = reg_val
        elif isinstance(inst, InstructionLoadWord):
            vj = op1
            in_rob, rob_idx, reg_val = self.ref_RF.load_register(op2)
            if in_rob:
                val = self.ref_cb.get(rob_idx)
                if val is None:
                    qk = rob_idx
                else:
                    vk = val
            else:
                vk = reg_val
        elif isinstance(inst, InstructionStoreWord):
            vj = op1
            in_rob, rob_idx, reg_val = self.ref_RF.load_register(op2)
            if in_rob:
                val = self.ref_cb.get(rob_idx)
                if val is None:
                    qk = rob_idx
                else:
                    vk = val
            else:
                vk = reg_val
        elif isinstance(inst, InstructionBranchOnEqual):
            in_rob, rob_idx, reg_val = self.ref_RF.load_register(op1)
            if in_rob:
                val = self.ref_cb.get(rob_idx)
                if val is None:
                    qj = rob_idx
                else:
                    vj = val
            else:
                vj = reg_val
            in_rob, rob_idx, reg_val = self.ref_RF.load_register(op2)
            if in_rob:
                val = self.ref_cb.get(rob_idx)
                if val is None:
                    qk = rob_idx
                else:
                    vk = val
            else:
                vk = reg_val

        # instruction Jump do not require decoding in the RS actually
        return vj, vk, qj, qk

    def next_exec_entry(self) -> _RSEntry:
        next_inst = None
        for rs_inst_entry in self.queue:
            if rs_inst_entry.is_ready_for_exec():
                next_inst = rs_inst_entry
                break

        return next_inst

    def receive_wb(self, rob_idx, value):
        for idx, rs_entry in enumerate(self.queue):
            if rs_entry.q_j == rob_idx:
                rs_entry.v_j = value
                rs_entry.q_j = None
            if rs_entry.q_k == rob_idx:
                rs_entry.v_k = value
                rs_entry.q_k = None

    def pop_entry(self, rob_idx):
        for idx in range(len(self.queue)):
            if self.queue[idx].rob_idx == rob_idx:
                self.queue.pop(idx)
                return


class DataSegment:
    """
    Data Segment of the memory. Starts from 716 of the program and until the end of the file. But currently have fixed
    size of 10
    """

    def __init__(self, data_mem: Dict[int, Data]):
        self._table = data_mem
        self._mem_lock = {}
        for addr in data_mem:
            self._mem_lock[addr] = False

    def __str__(self):
        desc_str = 'Data Segment:\n' + '716:'

        for key in sorted(self._table.keys()):
            desc_str += '\t{}'.format(self._table[key].int_val)

        return desc_str + '\n'

    def mem_write(self, mem_addr: int, value: int):
        self._table[mem_addr].set_int_val(value)

    def mem_read(self, mem_addr: int) -> int:
        return self._table[mem_addr].int_val

    def mem_lock(self, mem_addr):
        return self._mem_lock[mem_addr]


class ReorderBuffer:
    """
    6 entries
    """

    def __init__(self, RF: RegisterFile, DS: DataSegment):
        self.size = 6
        self._ref_RF = RF
        self._table = OrderedDict()
        self._ref_mem = DS

    def __str__(self):
        desc_str = 'ROB:\n'
        for idx, (cycle, rob_entry) in enumerate(self._table.items()):
            desc_str += '[{}]\n'.format(rob_entry.pip_inst.inst.desc_str)
        # end for
        return desc_str

    def avbl(self):
        avbl = False
        if len(self._table) < self.size:
            avbl = True
        # end if
        return avbl

    def flush_from(self, start_point: int):
        for idx, pip_inst in enumerate(list(self.queue)):
            if pip_inst.fetch_cycle > start_point:
                self.queue.pop(-1)

    def reset(self):
        self._table.clear()

    def execution(self, rob_idx: int, result):
        self._table[rob_idx].value = result
        pip_inst = self._table[rob_idx].pip_inst
        # Store, Jump, Branch, NOP and BREAK instructions bypass WB stage
        if pip_inst.get_type() == _InstTypes.BRCH or isinstance(pip_inst.inst, (
                InstructionStoreWord, InstructionNoOperation, InstructionBreakpoint)):
            self._table[rob_idx].status = _PipelineState.COMMIT
            if not isinstance(pip_inst.inst, InstructionStoreWord):
                self._table[rob_idx].ready_commit = True
        elif isinstance(pip_inst.inst, InstructionLoadWord):
            self._table[rob_idx].status = _PipelineState.MEMACCESS
            # self._table[rob_idx].ready_commit = True
        else:
            self._table[rob_idx].status = _PipelineState.WB

    def wb(self, rob_idx, value):
        rob_entry = self._table[rob_idx]

        if rob_entry.status == _PipelineState.MEMACCESS:
            if not self._ref_mem.mem_lock(value):
                rob_entry.status = _PipelineState.WB
                rob_entry.ready_commit = True
        else:
            rob_entry.status = _PipelineState.COMMIT
            rob_entry.ready_commit = True

        for idx, (key, entry) in enumerate(self._table.items()):
            if entry.dest_in_rob and entry.dest == rob_idx:
                entry.dest = value
                entry.dest_in_rob = False
                entry.ready_commit = True

    def add_entry(self, pip_inst: _PipelineInstEntry):
        self._table[pip_inst.issue_cycle] = _ROBEntry(pip_inst)
        if isinstance(pip_inst.inst, InstructionStoreWord):
            in_rob, rob_idx, reg_val = self._ref_RF.load_register(pip_inst.inst.dest)
            if in_rob:
                self._table[pip_inst.issue_cycle].dest_in_rob = True
                self._table[pip_inst.issue_cycle].dest = rob_idx
            else:
                self._table[pip_inst.issue_cycle].dest = reg_val
        elif isinstance(pip_inst.inst, (InstructionNoOperation, InstructionBreakpoint)):
            self._table[pip_inst.issue_cycle].status = _PipelineState.COMMIT
            self._table[pip_inst.issue_cycle].ready_commit = True

    def next_wb_entry(self, cycle):
        rob_entry = None
        for idx, (key, entry) in enumerate(self._table.items()):
            if entry.pip_inst.exec_cycle == cycle:
                continue
            if entry.status == _PipelineState.WB or (entry.status == _PipelineState.MEMACCESS):
                rob_entry = entry
                break
        return rob_entry

    def next_commit_entry(self):
        commit_entry = None
        for idx, (key, entry) in enumerate(self._table.items()):
            if isinstance(entry.pip_inst.inst, InstructionStoreWord) and not self._ref_mem.mem_lock(entry.value):
                entry.ready_commit = True
            if entry.status == _PipelineState.COMMIT and entry.ready_commit and idx == 0:
                commit_entry = entry
            break
        return commit_entry

    def lookup_rob_entry(self, rob_id) -> _ROBEntry:
        return self._table.get(rob_id)

    def pop_entry(self, rob_id):
        self._table.pop(rob_id, None)
